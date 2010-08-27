(ns format-conversion
  (:import [org.joda.time.format DateTimeFormatterBuilder])
  (:require [clj-time.core :as time])
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

(def two-digit-year
     (constant-semantics (factor= 2 (lit \y)) :yy))

(def four-digit-year
     (constant-semantics (factor= 4 (lit \y)) :yyyy))

(def hour-number-without-leading-zero
     (constant-semantics (lit \h) :h))

(def hour-number-with-leading-zero
     (constant-semantics (factor= 2 (lit \h)) :hh))

(def minutes-without-leading-zero
     (constant-semantics (lit \n) :n))

(def minutes-with-leading-zero
     (constant-semantics (factor= 2 (lit \n)) :nn))

(def seconds-without-leading-zero
     (constant-semantics (lit \s) :s))

(def seconds-with-leading-zero
     (constant-semantics (factor= 2 (lit \s)) :ss))

(defn format-pattern [token]
  (condp = token
      :d #(.appendDayOfMonth % 1)
      :dd #(.appendDayOfMonth % 2)
      :ddd #(.appendDayOfWeekShortText %)
      :dddd #(.appendDayOfWeekText %)
      :ddddd #(doto %
                (.appendDayOfMonth 2)
                (.appendLiteral "/")
                (.appendMonthOfYear 2)
                (.appendLiteral "/")
                (.appendYear 4 4))
      :dddddd #(doto %
                 (.appendDayOfWeekText)
                 (.appendLiteral ", ")
                 (.appendDayOfMonth 1)
                 (.appendLiteral " ")
                 (.appendMonthOfYearText)
                 (.appendLiteral " ")
                 (.appendYear 4 4))

      :m #(.appendMonthOfYear % 1)
      :mm #(.appendMonthOfYear % 2)
      :mmm #(.appendMonthOfYearShortText %)
      :mmmm #(.appendMonthOfYearText %)

      :yy #(.appendTwoDigitYear % (- (time/year (time/now)) 30) false)
      :yyyy #(.appendYear % 4 4)

      :h #(.appendHourOfDay % 1)
      :hh #(.appendHourOfDay % 2)

      :n #(.appendMinuteOfHour % 1)
      :nn #(.appendMinuteOfHour % 2)

      :s #(.appendSecondOfMinute % 1)
      :ss #(.appendSecondOfMinute % 2)))

(defn parse-date-format
  "Converts the string date-format to a list of date format tokens."
  [date-format]
  (let [parser (rep* (alt locale-long-date-format
                          locale-short-date-format
                          day-of-week
                          abbreviated-day-of-week
                          day-number-with-leading-zero
                          day-number-without-leading-zero

                          month-name
                          abbreviated-month-name
                          month-number-with-leading-zero
                          month-number-without-leading-zero

                          four-digit-year
                          two-digit-year

                          hour-number-with-leading-zero
                          hour-number-without-leading-zero

                          minutes-with-leading-zero
                          minutes-without-leading-zero

                          seconds-with-leading-zero
                          seconds-without-leading-zero))]
    (first (parser {:remainder date-format}))))

(defn create-formatter
  "Creates a DateTimeFormatter with a format as described by tokens."
  [tokens]
  (.toFormatter (reduce (fn [builder action]
                          (doto builder
                            action))
                        (new DateTimeFormatterBuilder)
                        (map format-pattern tokens))))

