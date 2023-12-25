;; Execute with `clj -X main/run` from the parent directory.
(ns main
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def edge-specs
  "Lines of the form 'node: node node node'."
  (str/split-lines (slurp "example.txt")))

(defn parse-append-edge-spec [seed-edges edge-spec]
  "Parse the input into a symmetric map of node: neighbors."
  (let
    [[n0 others] (str/split edge-spec #": ")
     nodes (str/split others #" ")]
    (defn append-n0 [edges n1]
      (let
        [n0-old (get edges n0 {})
         n1-old (get edges n1 {})
         n0-new (assoc n0-old n1 1)
         n1-new (assoc n1-old n0 1)]
        (assoc edges n0 n0-new n1 n1-new)))
    (reduce append-n0 seed-edges nodes)))

(def initial-graph
  (reduce parse-append-edge-spec {} edge-specs))

; We implement Karger's algorithm, which contracts two nodes in the graph until
; we have only two nodes left, which have some probability of being a minimum
; cut of the graph. The exercise already tells us the size of the minimum cut,
; so we can just try until we find a cut of the right size, assuming the
; exercise chose the graph such that it has a unique minimum cut.

(defn contract [graph n0 n1]
  "Return a copy of the graph, with nodes n0 and n1 contracted as n2."
  (let
    [out-n0 (get graph n0)
     out-n1 (get graph n1)
     out-n2 (dissoc (merge-with + out-n0 out-n1) n0 n1)
     n2 (str n0 n1)
     ; At this point we have a map with the old two nodes removed, and replaced
     ; with the new nodes ... but the edges the other way still point at the old
     ; nodes, so we also need to visit the old outgoing edges and update them.
     graph-0 (assoc (dissoc graph n0 n1) n2 out-n2)
     contract-n01 (fn [edges] (assoc (dissoc edges n0 n1) n2 (+ (get edges n0 0) (get edges n1 0))))]
    (reduce
      (fn [g n-out] (update g n-out contract-n01))
      graph-0
      (keys out-n2))))

(defn karger-step [graph]
  (loop [g graph]
    (if (= 2 (count g))
      g
      (let
        [n0 (rand-nth (keys g))
         n1 (rand-nth (keys (get g n0)))]
        (recur (contract g n0 n1))))))

(defn karger-until [graph threshold]
  "Try random cuts until we find one of the threshold size."
  (loop []
    (let
      [cut (karger-step graph)
       cut-size (first (vals (first (vals cut))))]
      (if (= threshold cut-size) cut (recur)))))

(defn run [opts]
  (println "Graph" initial-graph)
  (println "Karger-n" (karger-until initial-graph 3)))
