(ns format-conversion-tests
  (:import [java.io StringWriter]
           [org.joda.time.format DateTimeFormatterBuilder])
  (:require [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:use clojure.test format-conversion))

(deftest sub-parser-tests
  (are [sub-parser state expected-result] (= [expected-result {:remainder nil}] (sub-parser {:remainder state}))
       locale-date-time "C" 'c
       locale-date-time "c" 'c

       day-number-without-leading-zero "D" 'd
       day-number-without-leading-zero "d" 'd
       day-number-with-leading-zero "DD" 'dd
       day-number-with-leading-zero "Dd" 'dd
       day-number-with-leading-zero "dd" 'dd
       abbreviated-day-of-week "DDD" 'ddd
       abbreviated-day-of-week "DDd" 'ddd
       abbreviated-day-of-week "ddd" 'ddd
       day-of-week "DDDD" 'dddd
       day-of-week "DDdd" 'dddd
       day-of-week "dddd" 'dddd
       locale-short-date-format "DDDDD" 'ddddd
       locale-short-date-format "DDDdd" 'ddddd
       locale-short-date-format "ddddd" 'ddddd
       locale-long-date-format "DDDDDD" 'dddddd
       locale-long-date-format "DDDddd" 'dddddd
       locale-long-date-format "dddddd" 'dddddd

       month-number-without-leading-zero "m" 'm
       month-number-without-leading-zero "M" 'm
       month-number-with-leading-zero "MM" 'mm
       month-number-with-leading-zero "Mm" 'mm
       month-number-with-leading-zero "mm" 'mm
       abbreviated-month-name "MMM" 'mmm
       abbreviated-month-name "MMm" 'mmm
       abbreviated-month-name "mmm" 'mmm
       month-name "MMMM" 'mmmm
       month-name "MMmm" 'mmmm
       month-name "mmmm" 'mmmm

       two-digit-year "YY" 'yy
       two-digit-year "Yy" 'yy
       two-digit-year "yy" 'yy
       four-digit-year "YYYY" 'yyyy
       four-digit-year "YYyy" 'yyyy
       four-digit-year "yyyy" 'yyyy

       hour-number-without-leading-zero "H" 'h
       hour-number-without-leading-zero "h" 'h
       hour-number-with-leading-zero "HH" 'hh
       hour-number-with-leading-zero "Hh" 'hh
       hour-number-with-leading-zero "hh" 'hh

       minutes-without-leading-zero "N" 'n
       minutes-without-leading-zero "n" 'n
       minutes-with-leading-zero "NN" 'nn
       minutes-with-leading-zero "Nn" 'nn
       minutes-with-leading-zero "nn" 'nn

       seconds-without-leading-zero "S" 's
       seconds-without-leading-zero "s" 's
       seconds-with-leading-zero "SS" 'ss
       seconds-with-leading-zero "Ss" 'ss
       seconds-with-leading-zero "ss" 'ss

       milliseconds-unpadded "Z" 'z
       milliseconds-unpadded "z" 'z
       milliseconds-padded "ZZZ" 'zzz
       milliseconds-padded "ZZz" 'zzz
       milliseconds-padded "zzz" 'zzz

       locale-short-time-format "T" 't
       locale-short-time-format "t" 't
       locale-long-time-format "TT" 'tt
       locale-long-time-format "Tt" 'tt
       locale-long-time-format "tt" 'tt

       long-half-day-specifier "AM/pm" 'am-pm
       long-half-day-specifier "Am/pm" 'am-pm
       long-half-day-specifier "am/pm" 'am-pm
       long-half-day-specifier "am/Pm" 'am-pm
       long-half-day-specifier "am/PM" 'am-pm
       short-half-day-specifier "A/p" 'a-p
       short-half-day-specifier "a/p" 'a-p
       short-half-day-specifier "a/P" 'a-p
       short-half-day-specifier "a/p" 'a-p
       locale-half-day-specifier "AMpm" 'ampm
       locale-half-day-specifier "Ampm" 'ampm
       locale-half-day-specifier "ampm" 'ampm
       locale-half-day-specifier "amPm" 'ampm
       locale-half-day-specifier "amPM" 'ampm

       julian-day-number "J" 'j
       julian-day-number "j" 'j

       text-literal "''" ""
       text-literal "'text'" "text"
       text-literal "'one\"two'" "one\"two"
       text-literal "\"\"" ""
       text-literal "\"text\"" "text"
       text-literal "\"one'two\"" "one'two"
       unterminated-text-literal "'foo" "foo"
       unterminated-text-literal "\"bar" "bar"

       implicit-text-literal ":" ":"
       implicit-text-literal "/" "/"
       ;; Catches *all*, including things that would be caught elsewhere.
       implicit-text-literal "d" "d")
  (is (= ["fo" {:remainder (seq "sho'")}] (text-literal {:remainder "'fo'sho'"})))
  (is (= ["fo" {:remainder (seq "sho\"")}] (text-literal {:remainder "\"fo\"sho\""})))
  (is (= ["1" {:remainder (seq "2")}] (implicit-text-literal {:remainder "12"})))
  (let [[result _] (julian-day-number {:remainder "J"})]
    (is (= 'j result))
    (is (= "J" (:input (meta result)))))
  (let [[result _] (long-half-day-specifier {:remainder "AM/pM"})]
    (is (= 'am-pm result))
    (is (= "AM/pM" (:input (meta result)))))
  (let [[result _] (short-half-day-specifier {:remainder "a/P"})]
    (is (= 'a-p result))
    (is (= "a/P" (:input (meta result)))))
  (let [[result _] (locale-half-day-specifier {:remainder "AmPm"})]
    (is (= 'ampm result))
    (is (= "AmPm" (:input (meta result))))))

(deftest date-format-parsing-tests
  (testing "basic parser behaviour"
    (are [expected actual] (= expected (parse-date-format actual))
         '[dd mm yy] "ddmmyy"

         '[h n z m] "hmzm"
         '[hh n z m] "hhmzm"
         '[h nn z mm] "hmmzmm"
         '[hh nn z mm] "hhmmzmm"
         '[h "" n] "h''m"

         '[hh clock-h n am-pm] "hhhmam/pm"
         '[hh clock-h n a-p] "hhhma/p"
         '[hh clock-h n ampm] "hhhmampm"
         '[hh clock-hh n am-pm] "hhhhmam/pm"
         '[hh clock-hh n a-p] "hhhhma/p"
         '[hh clock-hh n ampm] "hhhhmampm"
         '[j] "j"
         '["J" "j"] "Jj"
         '["j" "j"] "jj"))
  (are [expected actual] (= expected (parse-date-format actual))
       '[c] "c"
       '[d] "d"
       '[dd] "dd"
       '[ddd] "ddd"
       '[dddd] "dddd"
       '[ddddd] "ddddd"
       '[dddddd] "dddddd"
       '[m] "m"
       '[mm] "mm"
       '[mmm] "mmm"
       '[mmmm] "mmmm"
       '[yy] "yy"
       '[yyyy] "yyyy"
       '[h] "h"
       '[hh] "hh"
       '[n] "n"
       '[nn] "nn"
       '[s] "s"
       '[ss] "ss"
       '[z] "z"
       '[zzz] "zzz"
       '[t] "t"
       '[tt] "tt"
       '[am-pm] "am/pm"
       '[a-p] "a/p"
       '[ampm] "ampm"
       '[j] "j"
       '["foo"] "'foo'"
       '["bar"] "'bar")
  (testing "implicit text literals"
    (are [expected actual] (= expected (parse-date-format actual))
         [":"] ":"
         ["/"] "/"
         ["x"] "x"))
  (is (= '[d "o" n "t do this"] (parse-date-format "don't do this")) "Unterminated text literals"))

(deftest builder-updater-for-token-tests
  (let [applied-token (fn [token date]
                        (let [builder (new DateTimeFormatterBuilder)]
                          ((builder-updater-for-token token) builder)
                          (time-format/unparse (.toFormatter builder) date)))]
    (are [token expected-pattern] (= expected-pattern
                                     (applied-token token (time/from-time-zone (time/date-time 2010 8 2 9 1 5 9)
                                                                               (time/time-zone-for-id "Pacific/Auckland"))))
         'c "02/08/2010 9:01:05 a.m."

         'd "2"
         'dd "02"
         'ddd "Mon"
         'dddd "Monday"
         'ddddd "02/08/2010"
         'dddddd "Monday, 2 August 2010"

         'm "8"
         'mm "08"
         'mmm "Aug"
         'mmmm "August"

         'yy "10"
         'yyyy "2010"

         'h "9"
         'hh "09"
         'clock-h "9"
         'clock-hh "09"

         'n "1"
         'nn "01"

         's "5"
         'ss "05"

         'z "9"
         'zzz "009"

         't "9:01 a.m."
         'tt "9:01:05 a.m."

         (with-meta 'am-pm {:input "AM/pm"}) "AM"
         (with-meta 'am-pm {:input "Am/pm"}) "Am"
         (with-meta 'am-pm {:input "aM/pm"}) "aM"
         (with-meta 'am-pm {:input "am/pm"}) "am"
         (with-meta 'a-p {:input "A/p"}) "A"
         (with-meta 'a-p {:input "a/p"}) "a"
         (with-meta 'ampm {:input "AMpm"}) "A.M."
         (with-meta 'ampm {:input "Ampm"}) "A.m."
         (with-meta 'ampm {:input "aMpm"}) "a.M."
         (with-meta 'ampm {:input "ampm"}) "a.m."

         'j "2455410"

         "some literal text" "some literal text")
    (are [token expected-pattern] (= expected-pattern
                                     (applied-token token (time/from-time-zone (time/date-time 2009 8 2 21 1 5 9)
                                                                               (time/time-zone-for-id "Pacific/Auckland"))))
         'yy "09"

         'h "21"
         'hh "21"
         'clock-h "9"
         'clock-hh "09"

         't "9:01 p.m."
         'tt "9:01:05 p.m."

         (with-meta 'am-pm {:input "am/PM"}) "PM"
         (with-meta 'am-pm {:input "am/Pm"}) "Pm"
         (with-meta 'am-pm {:input "am/pM"}) "pM"
         (with-meta 'am-pm {:input "am/pm"}) "pm"
         (with-meta 'a-p {:input "a/P"}) "P"
         (with-meta 'a-p {:input "a/p"}) "p"
         (with-meta 'ampm {:input "amPM"}) "P.M."
         (with-meta 'ampm {:input "amPm"}) "P.m."
         (with-meta 'ampm {:input "ampM"}) "p.M."
         (with-meta 'ampm {:input "ampm"}) "p.m.")
    (are [token expected-pattern] (= expected-pattern
                                     (applied-token token (time/from-time-zone (time/date-time 2009 8 2)
                                                                               (time/time-zone-for-id "Pacific/Auckland"))))
         'c "02/08/2009"
         't "12:00 a.m."
         'tt "12:00:00 a.m.")
    (is (= "00" (applied-token 'ss (-> (time/date-time 2008 12 31 23 59 59)
                                       (.plusSeconds 1)
                                       (time/to-time-zone (time/time-zone-for-id "Pacific/Auckland")))))
        "Ensure leap seconds *aren't* handled.")))

(deftest formatter-creation-tests
  (let [test-date (time/from-time-zone (time/date-time 2010 8 2 9 1 5 9)
                                       (time/time-zone-for-id "Pacific/Auckland"))]
    (is (= "20100802" (time-format/unparse (create-formatter '[yyyy mm dd]) test-date)))))

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
       '[m h n m] '[m h m m]
       '[m hh n m] '[m hh m m]
       '[mm h nn mm] '[mm h mm mm]
       '[mm hh nn mm] '[mm hh mm mm]
       '[h "" n] '[h "" m]
       '[h "some long string" nn] '[h "some long string" mm]
       '[hh z "some long string" m] '[hh z "some long string" m]
       '[hh "several" "different" "strings" nn] '[hh "several" "different" "strings" mm]))

(deftest convert-hours-to-clockhours-tests
  (are [output input] (= output (convert-hours-to-clockhours input))
       '[h clock-h n am-pm] '[h h n am-pm]
       '[h clock-h n a-p] '[h h n a-p]
       '[h clock-h n ampm] '[h h n ampm]
       '[hh clock-hh n am-pm] '[hh hh n am-pm]
       '[hh clock-hh n a-p] '[hh hh n a-p]
       '[hh clock-hh n ampm] '[hh hh n ampm]))

(deftest convert-julian-day-number-to-text-literal-tests
  (are [output input] (= output (convert-julian-day-number-to-text-literal input))
       '[j] [(with-meta 'j {:input "j"})]
       '["j" "woohoo"] [(with-meta 'j {:input "j"}), (with-meta 'j {:input "woohoo"})]
       '["j" " "] [(with-meta 'j {:input "j"}) " "]))

(deftest julian-day-number-printer-tests
  (let [zone (time/time-zone-for-id "Pacific/Auckland")
        previous (time/from-time-zone (time/date-time 2010 8 30 23 59 59 999) zone)
        date (time/from-time-zone (time/date-time 2010 8 31) zone)
        chronology (.getChronology date)
        display-offset (* 43200 1000)
        millis (+ (.getMillis date) display-offset)
        locale nil
        printer (julian-day-number-printer)]
    (is (= 9 (.estimatePrintedLength printer)))
    (is (= "2455439" (let [buffer (new StringBuffer)]
                       (.printTo printer buffer millis chronology display-offset zone locale)
                       (str buffer))))
    (is (= "2455439" (let [writer (new StringWriter)]
                       (.printTo printer writer millis chronology display-offset zone locale)
                       (str writer))))
    (is (= "2455438" (let [buffer (new StringBuffer)]
                       (.printTo printer buffer (+ (.getMillis previous) display-offset) chronology display-offset zone locale)
                       (str buffer))))
    (is (thrown? UnsupportedOperationException
                 (.printTo printer (new StringBuffer) (.toLocalDateTime date) locale)))
    (is (thrown? UnsupportedOperationException
                 (.printTo printer (new StringWriter) (.toLocalDateTime date) locale))))
  (let [zone (time/time-zone-for-id "Europe/Paris")
        previous (time/from-time-zone (time/date-time 2010 8 30 13 59 59 999) zone)
        date (time/from-time-zone (time/date-time 2010 8 30 14) zone)
        chronology (.getChronology date)
        display-offset (* 2 3600 1000)
        locale nil
        printer (julian-day-number-printer)]
    (is (= "2455438" (let [buffer (new StringBuffer)]
                       (.printTo printer buffer (+ (.getMillis previous) display-offset) chronology display-offset zone locale)
                       (str buffer))))
    (is (= "2455439" (let [buffer (new StringBuffer)]
                       (.printTo printer buffer (+ (.getMillis date) display-offset) chronology display-offset zone locale)
                       (str buffer))))))

(deftest date-time-from-printer-input-tests
  (let [zone (time/time-zone-for-id "Pacific/Auckland")
        date (time/from-time-zone (time/date-time 2010 8 31 8 56 23) zone)
        offset (* 12 3600 1000)]
    (is (= (time/from-time-zone date zone) (date-time-from-printer-input (+ (.getMillis date) offset) offset zone)))))
