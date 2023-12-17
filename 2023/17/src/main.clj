;; Execute with `clj -X main/run` from the parent directory.
(ns main
  (:require [clojure.java.io :as io]))

(def input (slurp "example.txt"))

(defn run [opts]
  (println "The input is:")
  (println input))
