(ns format-conversion-tests
  (:import [java.io StringWriter]
           [org.joda.time.format DateTimeFormatterBuilder])
  (:require [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:use clojure.test format-conversion))

(deftest sub-parser-tests
  (are [sub-parser state expected-result] (= [expected-result {:remainder nil}] (sub-parser {:remainder state}))
       day-number-without-leading-zero "d" :d
       day-number-with-leading-zero "dd" :dd
       abbreviated-day-of-week "ddd" :ddd
       day-of-week "dddd" :dddd
       locale-short-date-format "ddddd" :ddddd
       locale-long-date-format "dddddd" :dddddd

       month-number-without-leading-zero "m" :m
       month-number-with-leading-zero "mm" :mm
       abbreviated-month-name "mmm" :mmm
       month-name "mmmm" :mmmm

       two-digit-year "yy" :yy
       four-digit-year "yyyy" :yyyy

       hour-number-without-leading-zero "h" :h
       hour-number-with-leading-zero "hh" :hh

       minutes-without-leading-zero "n" :n
       minutes-with-leading-zero "nn" :nn

       seconds-without-leading-zero "s" :s
       seconds-with-leading-zero "ss" :ss

       milliseconds-unpadded "z" :z
       milliseconds-padded "zzz" :zzz

       locale-short-time-format "t" :t
       locale-long-time-format "tt" :tt

       long-half-day-specifier "am/pm" :am/pm
       short-half-day-specifier "a/p" :a/p
       locale-half-day-specifier "ampm" :ampm))

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
  (let [applied-token (fn [token date]
                        (let [builder (new DateTimeFormatterBuilder)]
                          ((builder-updater-for-token token) builder)
                          (time-format/unparse (.toFormatter builder) date)))]
    (are [token expected-pattern] (= expected-pattern
                                     (applied-token token (time/from-time-zone (time/date-time 2010 8 2 9 1 5 9)
                                                                               (time/time-zone-for-id "Pacific/Auckland"))))
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
         :ampm "a.m.")
    (are [token expected-pattern] (= expected-pattern
                                     (applied-token token (time/from-time-zone (time/date-time 2009 8 2 21 1 5 9)
                                                                               (time/time-zone-for-id "Pacific/Auckland"))))
         :yy "09"

         :h "21"
         :hh "21"

         :t "9:01 p.m."
         :tt "9:01:05 p.m."

         :am/pm "pm"
         :a/p "p"
         :ampm "p.m.")))

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

