(ns covid-analysis.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defn get-csv-filenames [dir]
  (filter #(.endsWith % ".csv")
          (map str
               (filter #(.isFile %)
                       (file-seq (clojure.java.io/file dir))))))

(defn get-csv-dates [csv-filenames]
  (let [filenames (map #(last (str/split % #"/")) csv-filenames)]
    (map #(first (str/split % #"\.")) filenames)))

(defn to-int [s]
  (if (str/blank? s) 0 (int (Double/parseDouble s))))

(defn to-ints [row]
  (map to-int row))

(defn get-rows [file]
  (with-open [reader (io/reader file)]
    (doall
      (csv/read-csv reader))))

(def us-confirmed-data (get-rows us-confirmed-file))
(def us-deaths-data (get-rows us-deaths-file))
(def global-confirmed-data (get-rows global-confirmed-file))
(def global-deaths-data (get-rows global-deaths-file))
(def global-recovered-data (get-rows global-recovered-file))

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

(defn map-and-total-state [county-rows]
  (loop [rows (rest county-rows)
         states {}]
    (if (empty? rows)
      states
      (let [row (first rows)
            state (nth row 6)
            count (to-int (last row))]
        (recur (rest rows) (update states state #(+ count (if (nil? %) 0 %))))))))

(def confirmed-by-county (map-by-county us-confirmed-data))
(def deaths-by-county (map-by-county us-deaths-data))
(def confirmed-by-state (map-and-total-state us-confirmed-data))
(def deaths-by-state (map-and-total-state us-deaths-data))

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

(defn daily-delta [row n]
  (let [yesterday (take-last (inc n) row)
        today (take-last n row)]
    (map - today yesterday)))

(defn todays-increase [row]
  (let [today (to-int (last row))
        yesterday (to-int (last (butlast row)))
        increase (- today yesterday)]
    increase))

(defn get-hot-counties []
  (let [rows (rest us-confirmed-data)
        counties (map #(vector (nth % 10) (todays-increase %) (todays-increase (butlast %))) rows)
        counties (sort-by second counties)
        hot-counties (drop (- (count counties) 10) counties)]
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
        pct (double (/ diff prev-element))]
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

(defn get-state-case-trajectories []
  (let [state-map (gather-states (rest us-confirmed-data))
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
                             trajectory (/ (double trajectory) 14)]
                         [state (last new-cases) case-per-100K changes-per-day trajectory]))]
    (sort-by last trajectories)))

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

(defn rank-counties [us-confirmed]
  (let [trajectories (get-county-trajectories us-confirmed)
        down-counties (count (filter #(neg? (last %)) trajectories))
        up-counties (- (count trajectories) down-counties)
        nil-counties (count (filter #(zero? (last %)) trajectories))
        marginal-counties (count (filter #(and (pos? (last %)) (<= (last %) 5)) trajectories))
        scary-counties (count (filter #(> (last %) 10) trajectories))]
    [down-counties up-counties nil-counties marginal-counties scary-counties]))

(defn rank-counties-for-days [us-confirmed days]
  (loop [us-confirmed us-confirmed ranks [] days days]
    (if (zero? days)
      ranks
      (recur (map butlast us-confirmed) (conj ranks (rank-counties us-confirmed)) (dec days)))))

(defn -main
  [& args]
  (println (get-last global-confirmed-data "Province/State"))
  (println "US-Confirmed:  " (get-stats (get-row global-confirmed-data "US")))
  (println "US-Deaths:     " (get-stats (get-row global-deaths-data "US")))
  (println "US-Recovered:  " (get-stats (get-row global-recovered-data "US")))
  (println "US-Tested      " (last (get-total-tests)))
  (println "NYC Confirmed: " (get-stats (get-row us-confirmed-data "New York City, New York, US")))
  (println "NYC Deaths:    " (get-stats (get-row us-deaths-data "New York City, New York, US")))
  (println "Cook Confirmed:" (get-stats (get-row us-confirmed-data "Cook, Illinois, US")))

  (println "\ncounty distribution")
  (doseq [[down-counties
         up-counties
         nil-counties
         marginal-counties
         scary-counties] (rank-counties-for-days (rest us-confirmed-data) 2)]
    (println "down:" down-counties ", up:" up-counties ", nil:" nil-counties ", marginal:" marginal-counties ", scary:" scary-counties)
    )

  (println "\nCurrent Confirmed US")
  (print-graph (get-current-confirmed-us 14))

  (println "\nHot counties")
  (doseq [county (get-hot-counties)]
    (println county))

  (println "\nState Case Trajectories")
  (doseq [[state new-cases cases-per-100K days-of-change trajectory] (get-state-case-trajectories)]
    (println state (format "{%d, %.2f}" new-cases cases-per-100K) days-of-change (format "<%.2f>" trajectory)))

  (println "\nState Cases per 100K")
  (doseq [[state new-cases cases-per-100K] (sort-by #(nth % 2) (get-state-case-trajectories))]
    (printf "%s {%d, %.2f}\n" state new-cases cases-per-100K))

  (println "\nHigh Trajectory counties")
  (doseq [[county trajectory] (take-last 10 (get-county-trajectories (rest us-confirmed-data)))]
    (printf "%s %.2f\n" county trajectory))

  ;(println "\nCounty Mortality Rates")
  ;(doseq [county-rate (get-county-mortality-rates)]
  ;  (printf "%s %.2f (%d/%d)\n" (first county-rate) (last county-rate) (second county-rate) (nth county-rate 2)))

  (println "\nState Case Mortality Rates")
  (doseq [[state deaths cases mortality] (get-state-mortality-rates)]
    (let [pop (state-population state)]
      (printf "%s %.2f (%d/%d)\n" state mortality deaths cases)))

  (println "\nState Deaths per 100K")
  (doseq [[state death-per-pop] (get-state-deaths-per-population)]
    (printf "%s %.2f\n" state (* 100000.0 death-per-pop)))

  (println "\nInteresting Counties")
  (println (county-trajectory "Lake, Illinois, US"))
  (println (county-trajectory "Cook, Illinois, US"))
  (println (county-trajectory "Kenosha, Wisconsin, US"))
  (println (county-trajectory "McHenry, Illinois, US"))
  (println (county-trajectory "Juneau, Wisconsin, US"))
  (println (county-trajectory "Sauk, Wisconsin, US"))
  (println (county-trajectory "Columbia, Wisconsin, US"))
  (println (county-trajectory "East Baton Rouge, Louisiana, US"))
  (println (county-trajectory "West Baton Rouge, Louisiana, US"))
  (println (county-trajectory "Maricopa, Arizona, US"))
  (println (county-trajectory "Spokane, Washington, US"))
  )
