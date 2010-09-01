(ns nz.co.robotines.JEXIDateFormatter
  (:gen-class
   :constructors {[String] []}
   :init init
   :methods [[format [Object] String]]
   :state state)
  (:import [java.util Date]
           [org.joda.time DateTime])
  (:use format-conversion))

(defn -init [format-string]
  [[] (create-formatter (parse-date-format format-string))])

(def instant-types #{Date DateTime Long})

(defn -format [this instant]
  (when-not (instant-types (type instant))
    (throw (new IllegalArgumentException "instant")))
  (.. this state (print (new DateTime instant))))
