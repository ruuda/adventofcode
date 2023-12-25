;; Execute with `clj -X main/run` from the parent directory.
(ns main
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def edge-specs
  "Lines of the form 'node: node node node'."
  (str/split-lines (slurp "example.txt")))

(defn lex-swap [n0 n1]
  "Return n0 and n1 in lexicographic order."
  (if (<= 0 (compare n0 n1)) [n0 n1] [n1 n0]))

(defn parse-append-edge-spec [seed-edges edge-spec]
  (let
    [[n0 others] (str/split edge-spec #": ")
     nodes (str/split others #" ")]
    (defn append-n0 [edges n1]
      (let
        [[m0 m1] (lex-swap n0 n1)
         m0-old (get edges m0 #{})
         m0-new (conj m0-old m1)]
        (assoc edges m0 m0-new)))
    (reduce append-n0 seed-edges nodes)))

(def initial-graph
  (reduce parse-append-edge-spec {} edge-specs))

(defn run [opts]
  (println "Graph" initial-graph))
