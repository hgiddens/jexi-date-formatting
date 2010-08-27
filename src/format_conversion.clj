(ns format-conversion
  (:import [org.joda.time.format DateTimeFormat])
  (:use name.choi.joshua.fnparse))

(def day-number-without-leading-zero
     (constant-semantics (lit \d) :d))

(def day-number-with-leading-zero
     (constant-semantics (factor= 2 (lit \d)) :dd))

(def abbreviated-day-of-week
     (constant-semantics (factor= 3 (lit \d)) :ddd))

(def day-of-week
     (constant-semantics (factor= 4 (lit \d)) :dddd))

(def locale-short-date-format
     (constant-semantics (factor= 5 (lit \d)) :ddddd))

(def locale-long-date-format
     (constant-semantics (factor= 6 (lit \d)) :dddddd))

(def month-number-without-leading-zero
     (constant-semantics (lit \m) :m))

(def month-number-with-leading-zero
     (constant-semantics (factor= 2 (lit \m)) :mm))

(def abbreviated-month-name
     (constant-semantics (factor= 3 (lit \m)) :mmm))

(def month-name
     (constant-semantics (factor= 4 (lit \m)) :mmmm))

(defn format-pattern [token]
  (condp = token
      :d "d"
      :dd "dd"
      :ddd "EEE"
      :dddd "EEEE"
      :ddddd "dd/MM/YYYY"
      :dddddd "EEEE, d MMMM YYYY"

      :m "M"
      :mm "MM"
      :mmm "MMM"
      :mmmm "MMMM"))

(defn format-from-date-format [date-format]
  (let [parser (rep* (alt locale-long-date-format
                          locale-short-date-format
                          day-of-week
                          abbreviated-day-of-week
                          day-number-with-leading-zero
                          day-number-without-leading-zero

                          month-name
                          abbreviated-month-name
                          month-number-with-leading-zero
                          month-number-without-leading-zero))
        result (first (parser {:remainder date-format}))]
    (DateTimeFormat/forPattern (apply str (map format-pattern result)))))
