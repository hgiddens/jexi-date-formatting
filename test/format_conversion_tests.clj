(ns format-conversion-tests
  (:import [java.io StringWriter]
           [org.joda.time.format DateTimeFormatterBuilder])
  (:require [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:use clojure.test format-conversion))

(deftest sub-parser-tests
  (are [sub-parser state expected-result] (= [expected-result {:remainder nil}] (sub-parser {:remainder state}))
       locale-date-time "c" :c

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
       locale-half-day-specifier "ampm" :ampm

       text-literal "''" ""
       text-literal "'text'" "text"
       text-literal "'one\"two'" "one\"two"
       text-literal "\"\"" ""
       text-literal "\"text\"" "text"
       text-literal "\"one'two\"" "one'two")
  (is (= ["fo" {:remainder (seq "sho'")}] (text-literal {:remainder "'fo'sho'"})))
  (is (= ["fo" {:remainder (seq "sho\"")}] (text-literal {:remainder "\"fo\"sho\""}))))

(deftest date-format-parsing-tests
  (testing "basic parser behaviour"
    (are [expected actual] (= expected (parse-date-format actual))
         [:dd :mm :yy] "ddmmyy"

         [:h :n :z :m] "hmzm"
         [:hh :n :z :m] "hhmzm"
         [:h :nn :z :mm] "hmmzmm"
         [:hh :nn :z :mm] "hhmmzmm"
         [:h ":" :n] "h':'m"

         [:hh :clock-h :n :am/pm] "hhhmam/pm"
         [:hh :clock-h :n :a/p] "hhhma/p"
         [:hh :clock-h :n :ampm] "hhhmampm"
         [:hh :clock-hh :n :am/pm] "hhhhmam/pm"
         [:hh :clock-hh :n :a/p] "hhhhma/p"
         [:hh :clock-hh :n :ampm] "hhhhmampm"))
  (are [expected actual] (= expected (parse-date-format actual))
       [:c] "c"
       [:d] "d"
       [:dd] "dd"
       [:ddd] "ddd"
       [:dddd] "dddd"
       [:ddddd] "ddddd"
       [:dddddd] "dddddd"
       [:m] "m"
       [:mm] "mm"
       [:mmm] "mmm"
       [:mmmm] "mmmm"
       [:yy] "yy"
       [:yyyy] "yyyy"
       [:h] "h"
       [:hh] "hh"
       [:n] "n"
       [:nn] "nn"
       [:s] "s"
       [:ss] "ss"
       [:z] "z"
       [:zzz] "zzz"
       [:t] "t"
       [:tt] "tt"
       [:am/pm] "am/pm"
       [:a/p] "a/p"
       [:ampm] "ampm"
       ["foo"] "'foo'"))

(deftest builder-updater-for-token-tests
  (let [applied-token (fn [token date]
                        (let [builder (new DateTimeFormatterBuilder)]
                          ((builder-updater-for-token token) builder)
                          (time-format/unparse (.toFormatter builder) date)))]
    (are [token expected-pattern] (= expected-pattern
                                     (applied-token token (time/from-time-zone (time/date-time 2010 8 2 9 1 5 9)
                                                                               (time/time-zone-for-id "Pacific/Auckland"))))
         :c "02/08/2010 9:01:05 a.m."

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
         :clock-h "9"
         :clock-hh "09"

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
         :ampm "a.m."

         "some literal text" "some literal text")
    (are [token expected-pattern] (= expected-pattern
                                     (applied-token token (time/from-time-zone (time/date-time 2009 8 2 21 1 5 9)
                                                                               (time/time-zone-for-id "Pacific/Auckland"))))
         :yy "09"

         :h "21"
         :hh "21"
         :clock-h "9"
         :clock-hh "09"

         :t "9:01 p.m."
         :tt "9:01:05 p.m."

         :am/pm "pm"
         :a/p "p"
         :ampm "p.m.")
    (are [token expected-pattern] (= expected-pattern
                                     (applied-token token (time/from-time-zone (time/date-time 2009 8 2)
                                                                               (time/time-zone-for-id "Pacific/Auckland"))))
         :c "02/08/2009"
         :t "12:00 a.m."
         :tt "12:00:00 a.m.")))

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

(deftest optional-time-printer-tests
  (testing "when the time should be shown"
    (let [date (time/from-time-zone (time/date-time 2010 8 2 0 0 0 1)
                                    (time/time-zone-for-id "Pacific/Auckland"))
          chronology (.getChronology date)
          display-offset (* 43200 1000)
          millis (+ (.getMillis date) display-offset)
          display-zone (time/time-zone-for-id "Pacific/Auckland")
          locale nil
          printer (optional-time-printer)]
      (is (= 14 (.estimatePrintedLength printer)))
      (is (= " 12:00:00 a.m." (let [buffer (new StringBuffer)]
                                (.printTo printer buffer millis chronology display-offset display-zone locale)
                                (str buffer))))
      (is (= " 12:00:00 a.m." (let [writer (new StringWriter)]
                                (.printTo printer writer millis chronology display-offset display-zone locale)
                                (str writer))))
      (is (= " 12:00:00 a.m." (let [buffer (new StringBuffer)]
                                (.printTo printer buffer (.toLocalDateTime date) locale)
                                (str buffer))))
      (is (= " 12:00:00 a.m." (let [writer (new StringWriter)]
                                (.printTo printer writer (.toLocalDateTime date) locale)
                                (str writer))))))
  (testing "when the time should be omitted"
    (let [date (time/from-time-zone (time/date-time 2010 8 2) (time/time-zone-for-id "Pacific/Auckland"))
          chronology (.getChronology date)
          display-offset (* 43200 1000)
          millis (+ (.getMillis date) display-offset)
          display-zone (time/time-zone-for-id "Pacific/Auckland")
          locale nil
          printer (optional-time-printer)]
      (is (= 14 (.estimatePrintedLength printer)))
      (is (= "" (let [buffer (new StringBuffer)]
                  (.printTo printer buffer millis chronology display-offset display-zone locale)
                  (str buffer))))
      (is (= "" (let [writer (new StringWriter)]
                  (.printTo printer writer millis chronology display-offset display-zone locale)
                  (str writer))))
      (is (= "" (let [buffer (new StringBuffer)]
                  (.printTo printer buffer (.toLocalDateTime date) locale)
                  (str buffer))))
      (is (= "" (let [writer (new StringWriter)]
                  (.printTo printer writer (.toLocalDateTime date) locale)
                  (str writer))))
      (is (= "" (let [buffer (new StringBuffer)]
                  (.printTo printer buffer (.toLocalDate date) locale)
                  (str buffer)))))))

(deftest convert-months-to-minutes-tests
  (are [output input] (= output (convert-months-to-minutes input))
       [:m :h :n :m] [:m :h :m :m]
       [:m :hh :n :m] [:m :hh :m :m]
       [:mm :h :nn :mm] [:mm :h :mm :mm]
       [:mm :hh :nn :mm] [:mm :hh :mm :mm]
       [:h ":" :n] [:h ":" :m]
       [:h "some long string" :nn] [:h "some long string" :mm]
       [:hh :z "some long string" :m] [:hh :z "some long string" :m]
       [:hh "several" "different" "strings" :nn] [:hh "several" "different" "strings" :mm]))

(deftest convert-hours-to-clockhours-tests
  (are [output input] (= output (convert-hours-to-clockhours input))
       [:h :clock-h :n :am/pm] [:h :h :n :am/pm]
       [:h :clock-h :n :a/p] [:h :h :n :a/p]
       [:h :clock-h :n :ampm] [:h :h :n :ampm]
       [:hh :clock-hh :n :am/pm] [:hh :hh :n :am/pm]
       [:hh :clock-hh :n :a/p] [:hh :hh :n :a/p]
       [:hh :clock-hh :n :ampm] [:hh :hh :n :ampm]))
