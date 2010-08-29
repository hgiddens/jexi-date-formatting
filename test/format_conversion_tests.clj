(ns format-conversion-tests
  (:import [java.io StringWriter]
           [org.joda.time.format DateTimeFormatterBuilder])
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
       seconds-with-leading-zero "ssrest" :ss "rest"

       milliseconds-unpadded "zrest" :z "rest"
       milliseconds-padded "zzzrest" :zzz "rest"

       locale-short-time-format "trest" :t "rest"
       locale-long-time-format "ttrest" :tt "rest"

       long-half-day-specifier "am/pmrest" :am/pm "rest"
       short-half-day-specifier "a/prest" :a/p "rest"
       locale-half-day-specifier "ampmrest" :ampm "rest"))

(deftest date-format-parsing-tests
  (is (= [:dd :mm :yy] (parse-date-format "ddmmyy")))
  (testing "precedence is correct for potentially ambiguous parses"
    (are [expected actual] (= expected (parse-date-format actual))
         [:dddddd] "dddddd"
         [:ddddd] "ddddd"
         [:dddd] "dddd"
         [:ddd] "ddd"
         [:dd] "dd"
         [:mmmm] "mmmm"
         [:mmm] "mmm"
         [:mm] "mm"
         [:yyyy] "yyyy"
         [:nn] "nn"
         [:ss] "ss"
         [:zzz] "zzz"
         [:tt] "tt"
         [:h :n :z :m] "hmzm"
         [:hh :n :z :m] "hhmzm"
         [:h :nn :z :mm] "hmmzmm"
         [:hh :nn :z :mm] "hhmmzmm")))

(deftest builder-updater-for-token-tests
  (let [test-date (time/from-time-zone (time/date-time 2010 8 2 9 1 5 9)
                                       (time/time-zone-for-id "Pacific/Auckland"))]
    (are [token expected-pattern] (= expected-pattern (let [builder (new DateTimeFormatterBuilder)]
                                                        ((builder-updater-for-token token) builder)
                                                        (time-format/unparse (.toFormatter builder) test-date)))
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
         :ss "05"

         :z "9"
         :zzz "009"

         :t "9:01 a.m."
         :tt "9:01:05 a.m."

         :am/pm "am"
         :a/p "a"
         :ampm "a.m.")))

(deftest formatter-creation-tests
  (let [test-date (time/from-time-zone (time/date-time 2010 8 2 9 1 5 9)
                                       (time/time-zone-for-id "Pacific/Auckland"))]
    (is (= "20100802" (time-format/unparse (create-formatter [:yyyy :mm :dd]) test-date)))))

(deftest custom-halfday-printer-tests
  (let [test-date (time/from-time-zone (time/date-time 2010 8 2 9 1 5 9)
                                       (time/time-zone-for-id "Pacific/Auckland"))
        chronology (.getChronology test-date)
        display-offset 0
        display-zone nil
        locale (java.util.Locale/getDefault)
        printer (custom-halfday-printer "ante meridiem" "post meridiem")]
    (is (= 13 (.estimatePrintedLength printer)))
    (is (= "ante meridiem" (let [buffer (new StringBuffer)]
                    (.printTo printer buffer (.getMillis test-date) chronology display-offset display-zone locale)
                    (str buffer))))
    (is (= "post meridiem" (let [writer (new StringWriter)]
                    (.printTo printer writer (+ (.getMillis test-date) 43200000) chronology display-offset display-zone locale)
                    (str writer))))
    (is (= "ante meridiem" (let [buffer (new StringBuffer)]
                    (.printTo printer buffer (.toLocalDateTime test-date) locale)
                    (str buffer))))
    (is (= "ante meridiem" (let [writer (new StringWriter)]
                    (.printTo printer writer (.toLocalDateTime test-date) locale)
                    (str writer))))))

