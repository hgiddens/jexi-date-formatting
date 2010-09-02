(ns interface-tests
  (:import [java.util Calendar Date]
           [nz.co.robotines JEXIDateFormatter])
  (:use clojure.test
        [clj-time.core :only [date-time from-time-zone time-zone-for-id]]
        [clj-time.coerce :only [to-long]]))

(def tz (time-zone-for-id "Pacific/Auckland"))

(deftest date-formatting-tests
  (let [date (from-time-zone (date-time 2010 9 1 23 27 45 7) tz)
        formatter (new JEXIDateFormatter "ddd mmm dd hh:mm:ss 'NZST' yyyy")]
    (are [date-object] (= "Wed Sep 01 23:27:45 NZST 2010" (.. formatter (format date-object)))
         date ; org.joda.time.DateTime
         (new Date (to-long date)) ; java.util.Date
         (to-long date)) ; java.lang.Long
    (is (thrown? IllegalArgumentException (.. formatter (format (int 0)))))
    (is (thrown? IllegalArgumentException (.. formatter (format (Calendar/getInstance)))))
    (are [expected format-string] (= expected (.. (new JEXIDateFormatter format-string) (format date)))
         "1/09/2010 11:27:45 p.m." "c"

         "1" "d"
         "01" "dd"
         "Wed" "ddd"
         "Wednesday" "dddd"
         "1/09/2010" "ddddd"
         "Wednesday, 1 September 2010" "dddddd"

         "9" "m"
         "09" "mm"
         "Sep" "mmm"
         "September" "mmmm"
         "23:27" "h:mm"
         "23109" "hdmm"

         "10" "yy"
         "2010" "yyyy"

         "23" "h"
         "23" "hh"
         "11p.m." "hampm"

         "27" "n"
         "27" "nn"

         "45" "s"
         "45" "ss"

         "7" "z"
         "007" "zzz"

         "11:27 p.m." "t"
         "11:27:45 p.m." "tt"

         "pm" "am/pm"
         "p" "a/p"
         "p.m." "ampm"
         "p.M." "ampM"

         "2455440" "J"
         "JJ" "JJ"

         "foo" "\"foo\""
         "bar" "'bar'"
         "qq" "qq")
    (is (= "1/09/2010" (.format (new JEXIDateFormatter "c") (from-time-zone (date-time 2010 9 1) tz))))))
