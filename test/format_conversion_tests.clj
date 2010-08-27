(ns format-conversion-tests
  (:require [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:use clojure.test format-conversion))

(deftest sub-parser-tests
  (are [sub-parser state expected-result expected-state] (let [[result new-state] (sub-parser {:remainder state})]
                                                           (and (= expected-result result)
                                                                (= expected-state (apply str (:remainder new-state)))))
       day-number-without-leading-zero "drest" :d "rest"
       day-number-with-leading-zero "ddrest" :dd "rest"
       abbreviated-day-of-week "dddrest" :ddd "rest"
       day-of-week "ddddrest" :dddd "rest"
       locale-short-date-format "dddddrest" :ddddd "rest"
       locale-long-date-format "ddddddrest" :dddddd "rest"))

(deftest to-format-pattern-tests
  (are [token expected-pattern] (= expected-pattern (format-pattern token))
       :d "d"
       :dd "dd"
       :ddd "EEE"
       :dddd "EEEE"
       :ddddd "dd/MM/YYYY"
       :dddddd "EEEE, d MMMM YYYY"))

(deftest basic-conversion-tests
  (let [test-date (time/from-time-zone (time/date-time 2010 8 2 9 1 5 9)
                                       (time/time-zone-for-id "Pacific/Auckland"))]
    (are [x y] (= x (time-format/unparse (format-from-date-format y) test-date))
         "2" "d"
         "02" "dd"
         "Mon" "ddd"
         "Monday" "dddd"
         "02/08/2010" "ddddd"
         "Monday, 2 August 2010" "dddddd"
         "Monday, 2 August 201002" "dddddddd")))
