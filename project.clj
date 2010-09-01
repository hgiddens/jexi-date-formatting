(defproject jexi-date-formatting "0.0.1"
  :description "Format dates according to JEXI date format specifiers"
  :aot [nz.co.robotines.JEXIDateFormatter]
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0-master-SNAPSHOT"]
                 [clj-time "0.1.0-SNAPSHOT"]
                 [org.clojars.hiredman/fnparse "2.2.4"]]
  :dev-dependencies [[swank-clojure "1.2.1"]])
