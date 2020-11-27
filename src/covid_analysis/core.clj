(ns covid-analysis.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def graph-width 100)
(def global-confirmed-file "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
(def global-deaths-file "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
(def global-recovered-file "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
(def us-confirmed-file "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
(def us-deaths-file "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
(def us-daily-reports-directory "../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports_us")


(def state-population
  {"Alabama" 4903185
   "Alaska" 731545
   "American Samoa" 55641
   "Arizona" 7278717
   "Arkansas" 3017825
   "California" 39512223
   "Colorado" 5758736
   "Connecticut" 3565287
   "Delaware" 973764
   "District of Columbia" 705749
   "Florida" 21477737
   "Georgia" 10617423
   "Guam" 165718
   "Hawaii" 1415872
   "Idaho" 1787065
   "Illinois" 12671821
   "Indiana" 6732219
   "Iowa" 3155070
   "Kansas" 2913314
   "Kentucky" 4467673
   "Louisiana" 4648794
   "Maine" 1344212
   "Maryland" 6045680
   "Massachusetts" 6949503
   "Michigan" 9986857
   "Minnesota" 5639632
   "Mississippi" 2976149
   "Missouri" 6137428
   "Montana" 1068778
   "Nebraska" 1934408
   "Nevada" 3080156
   "New Hampshire" 1359711
   "New Jersey" 8882190
   "New Mexico" 2096829
   "New York" 19453561
   "North Carolina" 10488084
   "North Dakota" 762062
   "Northern Mariana Islands" 55194
   "Ohio" 11689100
   "Oklahoma" 3956971
   "Oregon" 4217737
   "Pennsylvania" 12801989
   "Puerto Rico" 3193694
   "Rhode Island" 1059361
   "South Carolina" 5148714
   "South Dakota" 884659
   "Tennessee" 6833174
   "Texas" 28995881
   "U.S. Virgin Islands" 104914
   "Utah" 3205958
   "Vermont" 623989
   "Virginia" 8535519
   "Washington" 7614893
   "West Virginia" 1792147
   "Wisconsin" 5822434
   "Wyoming" 578759
   })

(defn to-int [s]
  (if (str/blank? s) 0 (int (Double/parseDouble s))))

(defn to-ints [row]
  (map to-int row))

(defn square [n] (* n n))

(defn mean [a] (/ (reduce + a) (count a)))

(defn stddev [a]
  (Math/sqrt (/
               (reduce + (map square (map - a (repeat (mean a)))))
               (- (count a) 1))))

(defn daily-delta [row n]
  (let [yesterday (take-last (inc n) row)
        today (take-last n row)]
    (map - today yesterday)))

(defn get-rows [file]
  (with-open [reader (io/reader file)]
    (doall
      (csv/read-csv reader))))

(defn get-csv-filenames [dir]
  (filter #(.endsWith % ".csv")
          (map str
               (filter #(.isFile %)
                       (file-seq (clojure.java.io/file dir))))))

(defn get-csv-dates [csv-filenames]
  (let [filenames (map #(last (str/split % #"/")) csv-filenames)]
    (map #(first (str/split % #"\.")) filenames)))

(defn date-string-to-date [date-string]
  (let [[month day year] (str/split date-string #"-")]
    (to-int (str year month day))))

(defn get-us-report-filenames-in-chronological-order []
  (let [files (get-csv-filenames us-daily-reports-directory)
        date-strings (get-csv-dates files)
        dates (map date-string-to-date date-strings)
        dated-filenames (map #(vector %1 %2) dates files)
        sorted-dated-filenames (sort-by first dated-filenames)]
    (map last sorted-dated-filenames)))

(defn get-filename-of-last-us-report []
  (last (get-us-report-filenames-in-chronological-order)))

(defn unpack-state-report [row]
  (let [state (nth row 0)
        confirmed (to-int (nth row 5))
        deaths (to-int (nth row 6))
        recovered (to-int (nth row 7))
        active (to-int (nth row 8))
        tested (to-int (nth row 11))
        hospitalized (to-int (nth row 12))
        state-report {:state state
                      :confirmed confirmed
                      :deaths deaths
                      :recovered recovered
                      :active active
                      :tested tested
                      :hospitalized hospitalized}]
    state-report
    ))

(defn get-us-report [filename]
  (let [rows (rest (get-rows filename))]
    (map unpack-state-report rows)))

(defn calculate-testing-statistics [state today-by-state yesterday-by-state]
  (let [today (today-by-state state)
        yesterday (yesterday-by-state state)
        confirmed (- (:confirmed today) (:confirmed yesterday))
        deaths (- (:deaths today) (:deaths yesterday))
        tested (- (:tested today) (:tested yesterday))
        hospitalized (- (:hospitalized today) (:hospitalized yesterday))]
    {:state state
     :confirmed confirmed
     :deaths deaths
     :tested tested
     :hospitalized hospitalized
     :positive-test-rate (if (zero? tested) 0.0 (double (/ confirmed tested)))
     :hospitalization-rate (if (zero? confirmed) 0.0 (double (/ hospitalized confirmed)))}))

(defn get-daily-testing-statistics-by-state []
  (let [last-two-file-names (take-last 2 (get-us-report-filenames-in-chronological-order))
        todays-stats (get-us-report (last last-two-file-names))
        yesterdays-stats (get-us-report (first last-two-file-names))
        today-by-state (into (hash-map) (map #(vector (:state %) %) todays-stats))
        yesterday-by-state (into (hash-map) (map #(vector (:state %) %) yesterdays-stats))
        todays-states (set (keys today-by-state))
        yesterdays-states (set (keys yesterday-by-state))
        states (set/intersection todays-states yesterdays-states)]
    (map #(calculate-testing-statistics % today-by-state yesterday-by-state) states)))


(defn add-state-to-hosp-map [hosp-map state]
  (let [state-name (:state state)
        hosp (:hospitalized state)
        hosps (hosp-map state-name)
        hosps (if (nil? hosps) [hosp] (conj hosps hosp))
        new-hosp-map (assoc hosp-map state-name hosps)]
    new-hosp-map))

(defn add-hosp [hosp-map us-report]
  (loop [hosp-map hosp-map states us-report]
    (if (empty? states)
      hosp-map
      (recur (add-state-to-hosp-map hosp-map (first states)) (rest states)))))

(defn get-state-hospitalizations [days]
  (let [file-names (take-last days (get-us-report-filenames-in-chronological-order))
        reports (map get-us-report file-names)]
    (loop [hosp-map {} reports reports]
      (if (empty? reports)
        hosp-map
        (recur (add-hosp hosp-map (first reports)) (rest reports))))))

(defn state-test-positive-rate [state-row]
  (let [state (first state-row)
        tests (to-int (nth state-row 11))
        cases (to-int (nth state-row 5))
        rate (if (zero? tests) 0.0 (double (/ cases tests)))]
    [state rate]))

(defn get-test-positive-rate-by-state []
  (let [last-report-filename (get-filename-of-last-us-report)
        last-report-rows (rest (get-rows last-report-filename))
        state-test-positive-rates (map state-test-positive-rate last-report-rows)]
    (sort-by last state-test-positive-rates)))

(def us-confirmed-data (get-rows us-confirmed-file))
(def us-deaths-data (get-rows us-deaths-file))
(def global-confirmed-data (get-rows global-confirmed-file))
(def global-deaths-data (get-rows global-deaths-file))
(def global-recovered-data (get-rows global-recovered-file))

;relative trajectories are 14 day average change in growth over average growth.
(defn get-global-relative-trajectories []
  (loop [countries [] global-data (rest global-confirmed-data)]
    (if (empty? global-data)
      (sort-by last countries)
      (let [row (first global-data)
            province (first row)
            country (second row)
            country (if (= "" province) country (str province "/" country))
            cases (to-ints (take-last 16 row))
            new-cases (daily-delta cases 15)
            mean-new-cases (double (mean new-cases))
            growth (daily-delta new-cases 14)
            trajectory (if (zero? mean-new-cases)
                         0.0
                         (double (/ (mean growth)
                                    mean-new-cases)))]
        (recur (conj countries [country mean-new-cases trajectory]) (rest global-data))))))

(defn total-tests [daily-report-csv-filename]
  (let [rows (rest (get-rows daily-report-csv-filename))
        tests (map #(to-int (nth % 11)) rows)]
    (reduce + tests)))

(defn get-total-tests []
  (let [csv-filenames (get-csv-filenames us-daily-reports-directory)
        dates (get-csv-dates csv-filenames)
        test-totals (map total-tests csv-filenames)
        dated-tests (map #(vector %1 %2) dates test-totals)]
    (sort-by last dated-tests)))

(defn map-by-county [county-rows]
  (apply hash-map (flatten (map #(vector (nth % 10) (to-int (last %))) (rest county-rows)))))

(defn map-and-total-state-value [county-rows value-f]
  (loop [rows (rest county-rows)
         states {}]
    (if (empty? rows)
      states
      (let [row (first rows)
            state (nth row 6)
            count (value-f row)]
        (recur (rest rows) (update states state #(+ count (if (nil? %) 0 %))))))))

(defn map-and-total-state [county-rows]
  (map-and-total-state-value county-rows #(to-int (last %))))

(defn new-deaths [row]
  (let [today (to-int (last row))
        yesterday (to-int (last (butlast row)))]
    (- today yesterday)))

(def confirmed-by-county (map-by-county us-confirmed-data))
(def deaths-by-county (map-by-county us-deaths-data))
(def confirmed-by-state (map-and-total-state us-confirmed-data))
(def deaths-by-state (map-and-total-state us-deaths-data))
(def new-deaths-by-state (map-and-total-state-value us-deaths-data new-deaths))

(defn get-county-mortality-rates []
  (let [qualified-confirmations (filter #(> (second %) 100) confirmed-by-county)
        counties (keys qualified-confirmations)
        counties (remove #(.startsWith % "Unassigned") counties)]
    (sort-by last
             (map #(vector %
                           (deaths-by-county %)
                           (confirmed-by-county %)
                           (* 100.0 (/ (deaths-by-county %) (max 1 (confirmed-by-county %)))))
                  counties))))

(defn get-state-mortality-rates []
  (let [states (keys confirmed-by-state)
        qualified-states (remove #(< (confirmed-by-state %) 100) states)]
    (sort-by last
             (map #(vector %
                           (deaths-by-state %)
                           (confirmed-by-state %)
                           (* 100.0 (/ (deaths-by-state %) (max 1 (confirmed-by-state %)))))
                  qualified-states))))

(defn deaths-per-population [[state deaths]]
  (let [pop (state-population state)]
    (double (/ deaths pop))))

(defn get-state-deaths-per-population []
  (let [states (filter #(contains? state-population (first %)) deaths-by-state)
        deaths-per-pops (map #(vector (first %) (deaths-per-population %)) states)]
    (sort-by last deaths-per-pops)))

(defn gather-states [data-rows]
  (loop [state-map {}
         rows data-rows]
    (if (empty? rows)
      state-map
      (let [county-row (first rows)
            state (nth county-row 6)
            county-header (take 11 county-row)
            county-data (to-ints (drop 11 county-row))
            county-row (concat county-header county-data)]
        (recur (update state-map state conj county-row) (rest rows))))))

(defn add-county [total county]
  (map + total county))

(defn total-counties [counties]
  (reduce add-county (map #(drop 11 %) counties)))

(defn total-states [state-map]
  (loop [totals {}
         states (keys state-map)]
    (if (empty? states)
      totals
      (let [state (first states)]
        (recur (assoc totals state (total-counties (state-map state)))
               (rest states))))))

(defn todays-increase [row]
  (let [today (to-int (last row))
        yesterday (to-int (last (butlast row)))
        increase (- today yesterday)]
    increase))

(defn get-hot-counties []
  (let [rows (rest us-confirmed-data)
        counties (map #(vector (nth % 10) (todays-increase %) (todays-increase (butlast %))) rows)
        counties (sort-by second counties)
        hot-counties (take-last 50 counties)]
    hot-counties))

(defn row-matches [name row]
  (not (empty? (filter #(= name %) row))))

(defn get-row [rows name]
  (first (filter #(row-matches name %) rows)))

(defn get-last [rows name]
  (last (get-row rows name)))

(defn get-stats [row]
  (let [last-element (to-int (last row))
        prev-element (to-int (last (butlast row)))
        diff (- last-element prev-element)
        pct (if (zero? prev-element) 0 (double (/ diff prev-element)))]
    [last-element prev-element diff pct]))

(defn round [x]
  (int (+ x 0.5)))

(defn get-line-width [day max-day]
  (let [star-volume (/ graph-width max-day)
        star (* day star-volume)]
    (round star)))

(defn make-line [width]
  (apply str (repeat width \*)))

(defn get-last-cells [row number-of-cells]
  (drop (- (count row) number-of-cells) row))

(defn print-lines [lines]
  (doseq [line lines] (println line)))

(defn print-graph [row]
  (let [max-day (apply max row)
        line-widths (map #(get-line-width % max-day) row)
        lines (map make-line line-widths)
        lines (map #(str %1 "   " %2) lines row)]
    (print-lines lines)))

(defn get-current-confirmed-us [days]
  (let [confirmed (to-ints (get-last-cells (get-row global-confirmed-data "US") days))
        deaths (to-ints (get-last-cells (get-row global-deaths-data "US") days))
        recovered (to-ints (get-last-cells (get-row global-recovered-data "US") days))]
    (map - confirmed deaths recovered)))

(defn get-state-case-trajectories-from [confirmed-data]
  (let [state-map (gather-states confirmed-data)
        state-totals (total-states state-map)
        states (keys state-totals)
        states (filter #(contains? state-population %) states)
        trajectories (for [state states]
                       (let [state-total (state-totals state)
                             new-cases (daily-delta state-total 16)
                             changes-per-day (daily-delta new-cases 14)
                             population (get state-population state 1)
                             cases (last state-total)
                             case-per-100K (* 100000.0 (/ cases population))
                             trajectory (reduce + changes-per-day)
                             trajectory (/ (double trajectory) 14)
                             trajectory-per-100K (* 100000.0 (/ trajectory population))]
                         {:state state
                          :new-cases (last new-cases)
                          :cases-per-100K case-per-100K
                          :changes-per-day changes-per-day
                          :trajectory trajectory
                          :trajectory-per-100K trajectory-per-100K}))]
    (sort-by :trajectory trajectories)))

(defn get-state-case-trajectories []
  (get-state-case-trajectories-from (rest us-confirmed-data)))

(defn get-trajectory-per-100K-history [n]
  (let [confirmed (rest us-confirmed-data)]
    (loop [n n trajs [] sigmas []]
      (if (zero? n)
        {:trajectories trajs :sigmas sigmas}
        (let [old-confirmed (map #(drop-last n %) confirmed)
              traj-history (get-state-case-trajectories-from old-confirmed)
              traj-history (map :trajectory-per-100K traj-history)]
          (recur (dec n)
                 (conj trajs (mean traj-history))
                 (conj sigmas (stddev traj-history))))))))

(defn get-county-trajectories [us-confirmed]
  (let [state-map (gather-states us-confirmed)
        counties (apply concat (vals state-map))
        counties (remove #(.startsWith (nth % 10) "Unassigned") counties)
        trajectories (for [county counties]
                       (let [cases (drop 11 county)
                             new-cases (daily-delta cases 16)
                             trajectory (daily-delta new-cases 14)]
                         [(nth county 10) (/ (reduce + trajectory) 14.0)]))]
    (sort-by last trajectories)))

(defn county-trajectory [county-name]
  (let [county (first (filter #(row-matches county-name %) us-confirmed-data))
        county-name (nth county 10)
        cases (to-int (last county))
        new-cases (- cases (to-int (last (butlast county))))
        county-data (to-ints (take-last 16 county))
        daily-change (daily-delta (daily-delta county-data 15) 14)
        trajectory (/ (reduce + daily-change) 14.0)]
    [county-name cases new-cases daily-change trajectory]))

(defn row-growth [row]
  (let [yesterday (drop-last row)
        today (drop 1 row)
        growth (map - today yesterday)]
    growth))

(defn model-recovery [confirmed]
  (let [growth (row-growth confirmed)
        recovered (reduce + (drop-last 14 growth))]
    (- (last confirmed) recovered)))

(defn active-cases-by-county []
  (let [counties (drop 1 us-confirmed-data)]
    (for [county counties]
      (let [name (nth county 10)
            row (to-ints (drop 11 county))
            active-cases (model-recovery row)]
        [name active-cases]))))

(defn rank-counties [us-confirmed]
  (let [trajectories (get-county-trajectories us-confirmed)
        total-counties (count trajectories)
        down-counties (count (filter #(neg? (last %)) trajectories))
        up-counties (- total-counties down-counties)
        nil-counties (count (filter #(zero? (last %)) trajectories))
        marginal-counties (count (filter #(and (pos? (last %)) (<= (last %) 5)) trajectories))
        scary-counties (count (filter #(> (last %) 10) trajectories))
        moderate-counties (- up-counties nil-counties marginal-counties scary-counties)]
    {:down-counties down-counties
     :up-counties up-counties
     :nil-counties nil-counties
     :marginal-counties marginal-counties
     :moderate-counties moderate-counties
     :scary-counties scary-counties}))

(defn rank-counties-for-days [us-confirmed days]
  (loop [us-confirmed us-confirmed ranks [] days days]
    (if (zero? days)
      ranks
      (recur (map butlast us-confirmed) (conj ranks (rank-counties us-confirmed)) (dec days)))))

(defn -main
  [& _]
  (println (get-last global-confirmed-data "Province/State"))
  (println "US-Confirmed:  " (get-stats (get-row global-confirmed-data "US")))
  (println "US-Deaths:     " (get-stats (get-row global-deaths-data "US")))
  (println "US-Recovered:  " (get-stats (get-row global-recovered-data "US")))
  (println "US-Tested      " (last (get-total-tests)))

  (println "\ncounty distribution")
  (doseq [{:keys [down-counties up-counties nil-counties marginal-counties scary-counties moderate-counties]}
          (rank-counties-for-days (rest us-confirmed-data) 2)]
    (println "down:" down-counties
             ", up:" up-counties
             ", zero:" nil-counties
             ", <5:" marginal-counties
             ", 6-10:" moderate-counties
             ", >10:" scary-counties)
    )

  (println "\nCurrent Confirmed US")
  (print-graph (get-current-confirmed-us 14))

  (println "\nHot counties")
  (doseq [county (get-hot-counties)]
    (println county))

  (println "\nStates with most new deaths")
  (let [top-new-deaths (sort-by last (map #(vector (key %) (val %)) new-deaths-by-state))]
    (doseq [state-new-death top-new-deaths]
      (printf "%s %d\n" (first state-new-death) (second state-new-death))))

  (def state-trajectories (get-state-case-trajectories))

  (println "\nStates not reporting")
  (let [non-reporters (filter #(zero? (:new-cases %)) state-trajectories)]
    (doseq [non-reporter non-reporters]
      (println (:state non-reporter))))

  (println "\nBacklog dumps [state new-cases last-delta]")
  (let [states (for [{:keys [state new-cases changes-per-day]} state-trajectories]
                 (let [last-delta (last changes-per-day)]
                   [state new-cases last-delta]))]
    (doseq [[_ new-cases delta :as state] states]
      (when (> (* 2 delta) new-cases)
        (println state))))

  (println "\nState Case Trajectories")
  (doseq [{:keys [state new-cases cases-per-100K changes-per-day trajectory]} state-trajectories]
    (println state (format "{%d, %.2f}" new-cases cases-per-100K) changes-per-day (format "<%.2f>" trajectory)))

  (println "\nHigh Trajectory counties")
  (doseq [[county trajectory] (take-last 10 (get-county-trajectories (rest us-confirmed-data)))]
    (printf "%s %.2f\n" county trajectory))

  (println "\nState Trajectories/100K stats")
  (def t-100K (map :trajectory-per-100K state-trajectories))
  (printf "mean: %.5f\n" (mean t-100K))
  (printf "sigma: %.5f\n" (stddev t-100K))

  ;(println "\ntrajectory/100K history")
  ;(println (get-trajectory-per-100K-history 60))

  (println "\nState Trajectories/100K")
  (doseq [{:keys [state trajectory-per-100K]} (sort-by :trajectory-per-100K state-trajectories)]
    (printf "%s %.2f\n" state trajectory-per-100K))

  (println "\nState {new cases Cases/100K}")
  (doseq [{:keys [state new-cases cases-per-100K]} (sort-by :cases-per-100K state-trajectories)]
    (printf "%s {%d, %.2f}\n" state new-cases cases-per-100K))


  ;(println "\nCounty Mortality Rates")
  ;(doseq [county-rate (get-county-mortality-rates)]
  ;  (printf "%s %.2f (%d/%d)\n" (first county-rate) (last county-rate) (second county-rate) (nth county-rate 2)))

  (println "\nState Case Mortality Rates")
  (doseq [[state deaths cases mortality] (get-state-mortality-rates)]
    (printf "%s %.2f (%d/%d)\n" state mortality deaths cases))

  (println "\nState Deaths per 100K")
  (let [deaths-per-pop-by-state (get-state-deaths-per-population)
        deaths-per-pop (map last deaths-per-pop-by-state)
        deaths-per-100K (map #(* 100000.0 %) deaths-per-pop)]
    (printf "mean %.5f\n" (mean deaths-per-100K))
    (printf "sigma %.5f\n\n" (stddev deaths-per-100K))
    (doseq [[state death-per-pop] deaths-per-pop-by-state]
      (printf "%s %.2f\n" state (* 100000.0 death-per-pop))))

  (println "\nTest Positive Rate by State")
  (doseq [[state rate] (get-test-positive-rate-by-state)]
    (printf "%s %.4f\n" state rate))

  (def daily-testing-stats (get-daily-testing-statistics-by-state))

  (println "\nDaily Test-positive Statistics")
  (def positive-rate (map :positive-test-rate daily-testing-stats))
  (printf "mean: %.5f\n" (mean positive-rate))
  (printf "sigma: %.5f\n" (stddev positive-rate))

  (println "\nStates daily test positive rates")
  (let [test-positives (sort-by :positive-test-rate daily-testing-stats)]
    (doseq [state test-positives]
      (printf "%s %.2f tested: %d, pos %d, hosp: %d, hosp-rate %.4f.\n"
              (:state state)
              (:positive-test-rate state)
              (:tested state)
              (:confirmed state)
              (:hospitalized state)
              (:hospitalization-rate state))))

  (println "\nStates with most tests today")
  (let [tested (take-last 10 (sort-by :tested daily-testing-stats))]
    (doseq [state tested]
      (printf "%s tested: %d, pos %d\n"
              (:state state)
              (:tested state)
              (:confirmed state))))

  ;(println "\nDaily Hospitalization Statistics")
  ;(def hosp-rate (map :hospitalization-rate daily-testing-stats))
  ;(printf "mean: %.5f\n" (mean hosp-rate))
  ;(printf "sigma: %.5f\n" (stddev hosp-rate))
  ;
  ;(println "\nStates daily hospitalization rates")
  ;(let [hospitalized (sort-by :hospitalization-rate (filter #(< (:hospitalization-rate %) 1) daily-testing-stats))]
  ;  (doseq [state hospitalized]
  ;    (printf "%s %.2f tested: %d, pos: %d, hosp %d.\n"
  ;            (:state state)
  ;            (:hospitalization-rate state)
  ;            (:tested state)
  ;            (:confirmed state)
  ;            (:hospitalized state))))

  (def country-trajectories (filter #(> (second %) 5000) (get-global-relative-trajectories)))
  (println "\nCountry relative 14 day trajectories")
  (println "[country mean-new-cases mean-growth]")
  (doseq [[country mean-new-cases trajectory] country-trajectories]
    (printf "%s (%.1f) %.4f%%\n" country mean-new-cases (* 100 trajectory)))

  (let [active-counties (sort-by last (active-cases-by-county))
        active-cases (map second active-counties)
        zero (count (filter #(<= % 0) active-cases))
        marginal (- (count (filter #(<= % 20) active-cases)) zero)
        moderate (- (count (filter #(<= % 1000) active-cases)) (+ zero marginal))
        active-cases-map (into (hash-map) active-counties)]
    (println "\nRecovery Model")
    (printf "Inactive counties: %d\n" zero)
    (printf "Marginal counties (<=200): %d\n" marginal)
    (printf "Moderate counties (<=1000): %d\n" moderate)
    (printf "Severe counties (>1000): %d\n" (- (count active-cases) zero marginal moderate))

    (println "\nCounties with most active cases")
    (doseq [[name cases] (take-last 10 active-counties)]
      (printf "%s: %d\n" name cases))

    (println "\nInteresting Counties")
    (def county-list ["Lake, Illinois, US"
                      "Cook, Illinois, US"
                      "Kenosha, Wisconsin, US"
                      "McHenry, Illinois, US"
                      "Juneau, Wisconsin, US"
                      "Sauk, Wisconsin, US"
                      "Columbia, Wisconsin, US"
                      "East Baton Rouge, Louisiana, US"
                      "West Baton Rouge, Louisiana, US"
                      "Maricopa, Arizona, US"
                      "Travis, Texas, US"
                      ])

    (doseq [county county-list]
      (println (county-trajectory county) " active:" (active-cases-map county))))

  ;(println "TEXAS HOSP: ", ((get-state-hospitalizations 14) "Texas"))
  ;(println "ARIZONA HOSP: ", ((get-state-hospitalizations 14) "Arizona"))

  )
