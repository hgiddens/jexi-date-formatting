(ns interface-tests
  (:import [java.util Calendar Date]
           [nz.co.robotines JEXIDateFormatter])
  (:use clojure.test
        [clj-time.core :only [date-time from-time-zone time-zone-for-id]]
        [clj-time.coerce :only [to-long]]))

(deftest date-formatting-tests
  (let [date (from-time-zone (date-time 2010 9 1 23 27 45) (time-zone-for-id "Pacific/Auckland"))
        formatter (new JEXIDateFormatter "ddd mmm dd hh:mm:ss 'NZST' yyyy")]
    (are [date-object] (= "Wed Sep 01 23:27:45 NZST 2010" (.. formatter (format date-object)))
         date ; org.joda.time.DateTime
         (new Date (to-long date)) ; java.util.Date
         (to-long date)) ; java.lang.Long
    (is (thrown? IllegalArgumentException (.. formatter (format (int 0)))))
    (is (thrown? IllegalArgumentException (.. formatter (format (Calendar/getInstance)))))))
