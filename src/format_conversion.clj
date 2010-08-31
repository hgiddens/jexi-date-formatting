(ns format-conversion
  (:import [org.joda.time DateTime DateTimeFieldType Days]
           [org.joda.time.chrono JulianChronology]
           [org.joda.time.format DateTimeFormatterBuilder DateTimePrinter])
  (:require [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:use [clojure.contrib.str-utils2 :only [lower-case]]
        name.choi.joshua.fnparse))

(def locale-date-time (constant-semantics (alt (lit \C) (lit \c)) 'c))

(def day-number-without-leading-zero (constant-semantics (alt (lit \D) (lit \d)) 'd))
(def day-number-with-leading-zero (constant-semantics (factor= 2 (alt (lit \D) (lit \d))) 'dd))
(def abbreviated-day-of-week (constant-semantics (factor= 3 (alt (lit \D) (lit \d))) 'ddd))
(def day-of-week (constant-semantics (factor= 4 (alt (lit \D) (lit \d))) 'dddd))
(def locale-short-date-format (constant-semantics (factor= 5 (alt (lit \D) (lit \d))) 'ddddd))
(def locale-long-date-format (constant-semantics (factor= 6 (alt (lit \D) (lit \d))) 'dddddd))

(def month-number-without-leading-zero (constant-semantics (alt (lit \M) (lit \m)) 'm))
(def month-number-with-leading-zero (constant-semantics (factor= 2 (alt (lit \M) (lit \m))) 'mm))
(def abbreviated-month-name (constant-semantics (factor= 3 (alt (lit \M) (lit \m))) 'mmm))
(def month-name (constant-semantics (factor= 4 (alt (lit \M) (lit \m))) 'mmmm))

(def two-digit-year (constant-semantics (factor= 2 (alt (lit \Y) (lit \y))) 'yy))
(def four-digit-year (constant-semantics (factor= 4 (alt (lit \Y) (lit \y))) 'yyyy))

(def hour-number-without-leading-zero (constant-semantics (alt (lit \H) (lit \h)) 'h))
(def hour-number-with-leading-zero (constant-semantics (factor= 2 (alt (lit \H) (lit \h))) 'hh))

(def minutes-without-leading-zero (constant-semantics (alt (lit \N) (lit \n)) 'n))
(def minutes-with-leading-zero (constant-semantics (factor= 2 (alt (lit \N) (lit \n))) 'nn))

(def seconds-without-leading-zero (constant-semantics (alt (lit \S) (lit \s)) 's))
(def seconds-with-leading-zero (constant-semantics (factor= 2 (alt (lit \S) (lit \s))) 'ss))

(def milliseconds-unpadded (constant-semantics (alt (lit \Z) (lit \z)) 'z))
(def milliseconds-padded (constant-semantics (factor= 3 (alt (lit \Z) (lit \z))) 'zzz))

(def locale-short-time-format (constant-semantics (alt (lit \T) (lit \t)) 't))
(def locale-long-time-format (constant-semantics (factor= 2 (alt (lit \T) (lit \t))) 'tt))

(def long-half-day-specifier (semantics (conc (alt (lit \A) (lit \a))
                                              (alt (lit \M) (lit \m))
                                              (lit \/)
                                              (alt (lit \P) (lit \p))
                                              (alt (lit \M) (lit \m)))
                                        #(with-meta 'am-pm {:input (apply str %)})))
(def short-half-day-specifier (semantics (conc (alt (lit \A) (lit \a))
                                               (lit \/)
                                               (alt (lit \P) (lit \p)))
                                         #(with-meta 'a-p {:input (apply str %)})))
(def locale-half-day-specifier (semantics (conc (alt (lit \A) (lit \a))
                                                (alt (lit \M) (lit \m))
                                                (alt (lit \P) (lit \p))
                                                (alt (lit \M) (lit \m)))
                                          #(with-meta 'ampm {:input (apply str %)})))

(def julian-day-number (semantics (alt (lit \J) (lit \j)) #(with-meta 'j {:input (str %)})))

(def text-literal (let [delimited-string (fn [delimiter]
                                           (complex [_ delimiter
                                                     contents (rep* (except anything delimiter))
                                                     _ delimiter]
                                             (apply str contents)))]
                    (alt (delimited-string (lit \')) (delimited-string (lit \")))))

(def implicit-text-literal (semantics anything str))

(defn custom-halfday-printer
  "Creates a DateTimePrinter that prints the halfday using the provided AM/PM strings."
  [am-string pm-string]
  (let [get-half-day (fn [chronology time locale]
                       (let [field (.getField (DateTimeFieldType/halfdayOfDay) chronology)
                             default (.getAsText field time locale)]
                         (condp = default
                             "AM" am-string
                             "PM" pm-string)))]
    (proxy [DateTimePrinter] []
      (estimatePrintedLength [] (max (count am-string) (count pm-string)))
      (printTo
       ([out partial locale]
          (.append out (get-half-day (.getChronology partial) partial locale)))
       ([out millis chronology display-offset display-zone locale]
          (.append out (get-half-day chronology millis locale)))))))

(defn date-time-from-printer-input
  "Creates a DateTime instance from the arguments provided to a DateTimePrinter's printTo method."
  [millis offset zone]
  (new DateTime (- millis offset) zone))

(defn optional-time-printer
  "A DateTimePrinter that prints the time if it is not exactly midnight in the specified zone.

The format used is ' 5:01:02 a.m.'. Note the leading space."
  []
  (let [formatter (-> (new DateTimeFormatterBuilder)
                      (.appendLiteral " ")
                      (.appendClockhourOfHalfday 1)
                      (.appendLiteral ":")
                      (.appendMinuteOfHour 2)
                      (.appendLiteral ":")
                      (.appendSecondOfMinute 2)
                      (.appendLiteral " ")
                      (.append (custom-halfday-printer "a.m." "p.m."))
                      .toFormatter)]
    (proxy [DateTimePrinter] []
      (estimatePrintedLength [] 14)
      (printTo
       ([out partial locale]
          ;; Unsupported field types are treated as being zero.
          (let [field-types [(DateTimeFieldType/hourOfDay)
                             (DateTimeFieldType/minuteOfHour)
                             (DateTimeFieldType/secondOfMinute)
                             (DateTimeFieldType/millisOfSecond)]
                absent? (fn [field-type]
                          (not (.isSupported partial field-type)))]
            (when (not-every? (fn [field-type]
                                (or (absent? field-type) (zero? (.get partial field-type))))
                              field-types)
              (.append out (.print formatter partial)))))
       ([out millis chronology display-offset display-zone locale]
          ;; This (the (- millis display-offset) part) is complete
          ;; guesswork, but it seems to do the right thing.
          (let [date (date-time-from-printer-input millis display-offset display-zone)]
            (when (not-every? zero? ((juxt time/hour time/minute time/sec time/milli) date))
              (.append out (time-format/unparse formatter date)))))))))

(def julian-epoch (new DateTime -4713 1 1 12 0 0 0 (JulianChronology/getInstanceUTC)))

(defn julian-day-number-printer
  "A DateTimePrinter that prints the Julian day number.

'Iulianum vocavimus: quia ad annum Iulianum dumtaxat accomodata est.'"
  []
  (proxy [DateTimePrinter] []
    (estimatePrintedLength [] 9)
    (printTo
     ([out partial locale]
        (throw (new UnsupportedOperationException "printTo")))
     ([out millis chronology display-offset display-zone locale]
        (->> (date-time-from-printer-input millis display-offset display-zone)
             (Days/daysBetween julian-epoch)
             .getDays
             str
             (.append out))))))

(defn builder-updater-for-token
  "Converts a format token to a function that updates a DateTimeFormatterBuilder."
  [token]
  (if (string? token)
    #(.appendLiteral % token)
    (condp = token
        'c #(doto %
              (.appendDayOfMonth 2)
              (.appendLiteral "/")
              (.appendMonthOfYear 2)
              (.appendLiteral "/")
              (.appendYear 4 4)
              (.append (optional-time-printer)))

        'd #(.appendDayOfMonth % 1)
        'dd #(.appendDayOfMonth % 2)
        'ddd #(.appendDayOfWeekShortText %)
        'dddd #(.appendDayOfWeekText %)
        'ddddd #(doto %
                  (.appendDayOfMonth 2)
                  (.appendLiteral "/")
                  (.appendMonthOfYear 2)
                  (.appendLiteral "/")
                  (.appendYear 4 4))
        'dddddd #(doto %
                   (.appendDayOfWeekText)
                   (.appendLiteral ", ")
                   (.appendDayOfMonth 1)
                   (.appendLiteral " ")
                   (.appendMonthOfYearText)
                   (.appendLiteral " ")
                   (.appendYear 4 4))

        'm #(.appendMonthOfYear % 1)
        'mm #(.appendMonthOfYear % 2)
        'mmm #(.appendMonthOfYearShortText %)
        'mmmm #(.appendMonthOfYearText %)

        'yy #(.appendTwoDigitYear % (- (time/year (time/now)) 30) false)
        'yyyy #(.appendYear % 4 4)

        'h #(.appendHourOfDay % 1)
        'hh #(.appendHourOfDay % 2)
        'clock-h #(.appendClockhourOfHalfday % 1)
        'clock-hh #(.appendClockhourOfHalfday % 2)

        'n #(.appendMinuteOfHour % 1)
        'nn #(.appendMinuteOfHour % 2)

        's #(.appendSecondOfMinute % 1)
        'ss #(.appendSecondOfMinute % 2)

        'z #(.appendMillisOfSecond % 1)
        'zzz #(.appendMillisOfSecond % 3)

        't #(doto %
              (.appendClockhourOfHalfday 1)
              (.appendLiteral ":")
              (.appendMinuteOfHour 2)
              (.appendLiteral " ")
              (.append (custom-halfday-printer "a.m." "p.m.")))
        'tt #(doto %
               (.appendClockhourOfHalfday 1)
               (.appendLiteral ":")
               (.appendMinuteOfHour 2)
               (.appendLiteral ":")
               (.appendSecondOfMinute 2)
               (.appendLiteral " ")
               (.append (custom-halfday-printer "a.m." "p.m.")))

        'am-pm #(.append % (custom-halfday-printer "am" "pm"))
        'a-p #(.append % (custom-halfday-printer "a" "p"))
        'ampm #(.append % (custom-halfday-printer "a.m." "p.m."))

        'j #(.append % (julian-day-number-printer)))))

(defn convert-months-to-minutes
  "Converts month specifiers in tokens to minute specifiers where appropriate."
  [tokens]
  (:result (reduce (fn [data raw-value]
                     (let [last-token-was-hour? (or (= (:last-token data) 'h) (= (:last-token data) 'hh))
                           value (cond (and (= raw-value 'm) last-token-was-hour?) 'n
                                       (and (= raw-value 'mm) last-token-was-hour?) 'nn
                                       :otherwise raw-value)]
                       (if (string? value)
                         (assoc data
                           :result (conj (:result data) value))
                         (assoc data
                           :result (conj (:result data) value)
                           :last-token value))))
                   {:result [], :last-token nil}
                   tokens)))

(defn convert-hours-to-clockhours
  "Converts hour specifiers in tokens to clock-hour specifiers where appropriate."
  [tokens]
  (let [is-halfday-specifier? '#{am-pm a-p ampm}
        is-hour-specifier? '#{h hh}
        hour-to-clockhour '{h clock-h, hh clock-hh}]
    (->> tokens
         reverse
         (reduce (fn [data raw-value]
                   (cond
                    (and (is-hour-specifier? raw-value) (:unconsumed data))
                    (assoc data
                      :result (conj (:result data) (hour-to-clockhour raw-value))
                      :unconsumed false)

                    (is-halfday-specifier? raw-value)
                    (assoc data
                      :result (conj (:result data) raw-value)
                      :unconsumed true)

                    :otherwise (assoc data :result (conj (:result data) raw-value))))
                 {:result [], :unconsumed false})
         :result
         reverse)))

(defn convert-julian-day-number-to-text-literal
  "Converts Julian day numbers to 'J' text literals, except for when they're the only token."
  [tokens]
  (if (= tokens '[j])
    tokens
    (map #(if (= % 'j) (:input (meta %)) %) tokens)))

(defn parse-date-format
  "Converts the string date-format to a list of date format tokens."
  [date-format]
  (let [parser (rep* (alt locale-date-time

                          locale-long-date-format
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
                          seconds-without-leading-zero

                          milliseconds-padded
                          milliseconds-unpadded

                          locale-long-time-format
                          locale-short-time-format

                          long-half-day-specifier
                          short-half-day-specifier
                          locale-half-day-specifier

                          julian-day-number

                          text-literal
                          ;; Implicit text literals must be last.
                          implicit-text-literal))]
    (-> (first (parser {:remainder date-format}))
        convert-months-to-minutes
        convert-hours-to-clockhours
        convert-julian-day-number-to-text-literal)))

(defn create-formatter
  "Creates a DateTimeFormatter with a format as described by tokens."
  [tokens]
  (.toFormatter (reduce (fn [builder action]
                          (doto builder
                            action))
                        (new DateTimeFormatterBuilder)
                        (map builder-updater-for-token tokens))))
