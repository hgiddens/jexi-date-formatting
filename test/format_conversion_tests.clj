(ns format-conversion-tests
  (:import [org.joda.time.format DateTimeFormatterBuilder])
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
       locale-long-date-format "ddddddrest" :dddddd "rest"

       month-number-without-leading-zero "mrest" :m "rest"
       month-number-with-leading-zero "mmrest" :mm "rest"
       abbreviated-month-name "mmmrest" :mmm "rest"
       month-name "mmmmrest" :mmmm "rest"

       two-digit-year "yyrest" :yy "rest"
       four-digit-year "yyyyrest" :yyyy "rest"

       hour-number-without-leading-zero "hrest" :h "rest"
       hour-number-with-leading-zero "hhrest" :hh "rest"

       minutes-without-leading-zero "nrest" :n "rest"
       minutes-with-leading-zero "nnrest" :nn "rest"

       seconds-without-leading-zero "srest" :s "rest"
       seconds-with-leading-zero "ssrest" :ss "rest"))

(deftest to-format-pattern-tests
  (let [test-date (time/from-time-zone (time/date-time 2010 8 2 9 1 5 9)
                                       (time/time-zone-for-id "Pacific/Auckland"))]
    (are [token expected-pattern] (let [builder (new DateTimeFormatterBuilder)]
                                    ((format-pattern token) builder)
                                    (let [result (time-format/unparse (.toFormatter builder) test-date)]
                                      (= expected-pattern result)))
         :d "2"
         :dd "02"
         :ddd "Mon"
         :dddd "Monday"
         :ddddd "02/08/2010"
         :dddddd "Monday, 2 August 2010"

         :m "8"
         :mm "08"
         :mmm "Aug"
         :mmmm "August"

         :yy "10"
         :yyyy "2010"

         :h "9"
         :hh "09"

         :n "1"
         :nn "01"

         :s "5"
         :ss "05")))

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
         "Monday, 2 August 201002" "dddddddd"

         "8" "m"
         "08" "mm"
         "Aug" "mmm"
         "August" "mmmm"

         "10" "yy"
         "2010" "yyyy"

         "9" "h"
         "09" "hh"

         "1" "n"
         "01" "nn"

         "5" "s"
         "05" "ss")))
