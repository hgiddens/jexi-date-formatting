(ns format-conversion
  (:import [org.joda.time DateTime DateTimeFieldType Days]
           [org.joda.time.chrono JulianChronology]
           [org.joda.time.format DateTimeFormatterBuilder DateTimePrinter])
  (:require [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:use [clojure.contrib.seq-utils :only [flatten]]
        [clojure.contrib.str-utils2 :only [lower-case]]
        name.choi.joshua.fnparse))

(declare create-formatter)

(defmacro simple-sub-parser [match-string result]
  `(semantics (conc ~@(map (fn [character]
                             `(alt (lit ~(Character/toUpperCase character))
                                   (lit ~(Character/toLowerCase character))))
                           match-string))
              (fn [parse#] (with-meta '~result {:input (apply str parse#)}))))

(def locale-date-time (simple-sub-parser "c" [d "/" mm "/" yyyy optional-time]))

(def day-number-without-leading-zero (simple-sub-parser "d" d))
(def day-number-with-leading-zero (simple-sub-parser "dd" dd))
(def abbreviated-day-of-week (simple-sub-parser "ddd" ddd))
(def day-of-week (simple-sub-parser "dddd" dddd))
(def locale-short-date-format (simple-sub-parser "ddddd" [d "/" mm "/" yyyy]))
(def locale-long-date-format (simple-sub-parser "dddddd" [dddd ", " d " " mmmm " " yyyy]))

(def month-number-without-leading-zero (simple-sub-parser "m" m))
(def month-number-with-leading-zero (simple-sub-parser "mm" mm))
(def abbreviated-month-name (simple-sub-parser "mmm" mmm))
(def month-name (simple-sub-parser "mmmm" mmmm))

(def two-digit-year (simple-sub-parser "yy" yy))
(def four-digit-year (simple-sub-parser "yyyy" yyyy))

(def hour-number-without-leading-zero (simple-sub-parser "h" h))
(def hour-number-with-leading-zero (simple-sub-parser "hh" hh))
(def minutes-without-leading-zero (simple-sub-parser "n" n))
(def minutes-with-leading-zero (simple-sub-parser "nn" nn))
(def seconds-without-leading-zero (simple-sub-parser "s" s))
(def seconds-with-leading-zero (simple-sub-parser "ss" ss))
(def milliseconds-unpadded (simple-sub-parser "z" z))
(def milliseconds-padded (simple-sub-parser "zzz" zzz))

(def locale-short-time-format (simple-sub-parser "t" [clock-h ":" nn " " #^{:input "ampm"} ampm]))
(def locale-long-time-format (simple-sub-parser "tt" [clock-h ":" nn ":" ss " " #^{:input "ampm"} ampm]))

(def long-half-day-specifier (simple-sub-parser "am/pm" am-pm))
(def short-half-day-specifier (simple-sub-parser "a/p" a-p))
(def locale-half-day-specifier (simple-sub-parser "ampm" ampm))

(def julian-day-number (simple-sub-parser "j" j))

(def text-literal (let [delimited-string (fn [delimiter]
                                           (complex [_ delimiter
                                                     contents (rep* (except anything delimiter))
                                                     _ delimiter]
                                             (apply str contents)))]
                    (alt (delimited-string (lit \')) (delimited-string (lit \")))))
(def unterminated-text-literal (let [unterminated-string (fn [delimiter]
                                                           (complex [_ delimiter
                                                                     contents (rep* anything)]
                                                             (apply str contents)))]
                                 (alt (unterminated-string (lit \')) (unterminated-string (lit \")))))

(def implicit-text-literal (semantics anything str))

(defn custom-halfday-printer
  "Creates a DateTimePrinter that prints the halfday using the provided AM/PM strings."
  [am-string pm-string]
  (proxy [DateTimePrinter] []
    (estimatePrintedLength [] (max (count am-string) (count pm-string)))
    (printTo
     ([out partial locale] (throw (new UnsupportedOperationException "printTo")))
     ([out millis chronology display-offset display-zone locale]
        (.append out (condp = (.. (DateTimeFieldType/halfdayOfDay) (getField chronology) (getAsText millis locale))
                         "AM" am-string
                         "PM" pm-string))))))

(defn date-time-from-printer-input
  "Creates a DateTime instance from the arguments provided to a DateTimePrinter's printTo method."
  [millis offset zone]
  (new DateTime (- millis offset) zone))

(defn optional-time-printer
  "A DateTimePrinter that prints the time if it is not exactly midnight in the specified zone.

The format used is ' 5:01:02 a.m.'. Note the leading space."
  []
  (let [formatter (create-formatter '[" " clock-h ":" nn ":" ss " " #^{:input "ampm"} ampm])]
    (proxy [DateTimePrinter] []
      (estimatePrintedLength [] 14)
      (printTo
       ([out partial locale] (throw (new UnsupportedOperationException "printTo")))
       ([out millis chronology display-offset display-zone locale]
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
     ([out partial locale] (throw (new UnsupportedOperationException "printTo")))
     ([out millis chronology display-offset display-zone locale]
        (->> (date-time-from-printer-input millis display-offset display-zone)
             (Days/daysBetween julian-epoch)
             .getDays
             str
             (.append out))))))

(defn input-for-token
  "Returns the input that generated token.

Used by the half-day specifiers to calculate the case of their output, and
by the Julian day number specifier to find the case of its
literalisation (when the token is not used as a Julian day number)."
  [token]
  (let [m (:input (meta token))]
    (assert (not (nil? m)))
    m))

(defn builder-updater-for-token
  "Converts a format token to a function that updates a DateTimeFormatterBuilder."
  [token]
  (if (string? token)
    #(.appendLiteral % token)
    (condp = token
        'optional-time #(.append % (optional-time-printer))

        'd #(.appendDayOfMonth % 1)
        'dd #(.appendDayOfMonth % 2)
        'ddd #(.appendDayOfWeekShortText %)
        'dddd #(.appendDayOfWeekText %)

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

        'am-pm #(.append % (let [original (input-for-token token)]
                             (custom-halfday-printer (subs original 0 2) (subs original 3))) )
        'a-p #(.append % (let [original (input-for-token token)]
                           (custom-halfday-printer (str (nth original 0)) (str (nth original 2)))))
        'ampm #(.append % (let [original (input-for-token token)]
                            (custom-halfday-printer (str (nth original 0) \. (nth original 1) \.)
                                                    (str (nth original 2) \. (nth original 3) \.))))

        'j #(.append % (julian-day-number-printer)))))

(defn apply-parser
  "Applies parser to the input.

Raises an AssertionError if the input doesn't match exactly."
  [parser input & state]
  (let [fail (fn [& args] (assert false))]
    (rule-match parser fail fail (apply assoc {} :remainder input state))))

(defn convert-months-to-minutes
  "Converts month specifiers in tokens to minute specifiers where appropriate."
  [tokens]
  (apply-parser (let [to-minutes '{m n, mm nn}
                      hour (invisi-conc (lit-alt-seq '[h hh]) (set-info :hour? true))
                      minute (complex [token (lit-alt-seq '[m mm])
                                       hour? (get-info :hour?)
                                       _ (set-info :hour? false)]
                               (if hour? (to-minutes token) token))
                      string (term string?)
                      specifier (invisi-conc anything (set-info :hour? false))]
                  (rep* (alt hour minute string specifier)))
                tokens :hour? false))

(defn convert-hours-to-clockhours
  "Converts hour specifiers in tokens to clock-hour specifiers where appropriate."
  [tokens]
  (apply-parser (let [to-clock-hour '{h clock-h, hh clock-hh}
                      half-day-specifier (lit-alt-seq '[am-pm a-p ampm])
                      hour (lit-alt-seq '[h hh])
                      trailing-half-day (conc (rep* (except anything (alt hour half-day-specifier))) half-day-specifier)
                      clock-hour (semantics (invisi-conc hour (followed-by trailing-half-day)) to-clock-hour)]
                  (rep* (alt clock-hour anything)))
                tokens))

(defn convert-julian-day-number-to-text-literal
  "Converts Julian day numbers to 'J' text literals, except for when they're the only token."
  [tokens]
  (if (= tokens '[j])
    tokens
    (map #(if (= % 'j) (input-for-token %) %) tokens)))

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
                          unterminated-text-literal
                          ;; Implicit text literals must be last.
                          implicit-text-literal))]
    (-> (apply-parser parser date-format)
        flatten
        convert-months-to-minutes
        convert-hours-to-clockhours
        convert-julian-day-number-to-text-literal)))

(defn create-formatter
  "Creates a DateTimeFormatter with a format as described by tokens.

The returned formatter will display times as in the Pacific/Auckland time zone."
  [tokens]
  (-> (reduce (fn [builder action]
                (doto builder
                  action))
              (new DateTimeFormatterBuilder)
              (map builder-updater-for-token tokens))
      .toFormatter
      (.withZone (time/time-zone-for-id "Pacific/Auckland"))))
