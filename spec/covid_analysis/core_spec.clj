(ns covid-analysis.core-spec
  (:require [speclj.core :refer :all]
            [covid-analysis.core :refer :all]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

;(describe "csv read"
;  (it "should should get last"
;    (should= "" (get-last global-confirmed "US"))))

(describe "round"
  (it "should round up at .5"
    (should= 0 (round 0))
    (should= 0 (round 0.1))
    (should= 1 (round 0.5))
    (should= 1 (round 0.8))
    ))

;(describe "covid-graph"
;  (it "get line width"
;    (should= 51 (get-line-width 22 30))
;    (should= 8 (get-line-width 15 140))))

(describe "the graph"
  (it "should create lines"
    (should= "" (make-line 0))
    (should= "*" (make-line 1))
    (should= "**" (make-line 2))))

(def row [1 2 3 4 5])
(describe "data gatherer"
  (it "Gets the right number of cells"
    (should= [] (get-last-cells row 0))
    (should= [5] (get-last-cells row 1))
    (should= [2 3 4 5] (get-last-cells row 4))))

(describe "state X state analysis"
  (let [data [[nil nil nil nil nil nil "state1" nil nil nil "s1c1" "1" "2"]
              [nil nil nil nil nil nil "state1" nil nil nil "s1c2" "3.0" "4.0"]
              [nil nil nil nil nil nil "state2" nil nil nil "s2c1" "5" "6"]
              [nil nil nil nil nil nil "state2" nil nil nil "s2c2" "7" "8"]]
        state-map (gather-states data)]
    (context "gather states"
      (it "should create a map with states as the key"
        (should (contains? state-map "state1"))
        (should= 2 (count (state-map "state1")))
        (should= #{["s1c1" 1 2] ["s1c2" 3 4]} (set (map #(take 3 (drop 10 %)) (state-map "state1"))))

        (should (contains? state-map "state2"))
        (should= 2 (count (state-map "state2")))
        (should= #{"s2c1" "s2c2"} (set (map #(nth % 10) (state-map "state2"))))))
    (context "total states"
      (it "should create a map of states with the rows totaled"
        (let [state-totals (total-states state-map)]
          (should= {"state1" [4 6] "state2" [12 14]} state-totals)))))
  (context "analysis math"
    (it "should find daily changes"
      (let [row [3 1 4 1 5 9 2 6]
            changes (daily-delta row 6)]
        (should= [3 -3 4 4 -7 4] changes))))

  )