;; Execute with `clj -X main/run` from the parent directory.
(ns main
  (:require [clojure.core.reducers :as r])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def wires
  "The wires that connect the different components."
  (str/split-lines (slurp "example2.txt")))

(defn parse-module [line]
  "Parse a module into [kind, name, outputs]."
  (let [[inputs outputs] (str/split line #" -> ")]
    { :kind (cond
               (str/starts-with? line "&") "&"
               (str/starts-with? line "%") "%"
               :else "i")
      :name (str/replace (str/replace inputs "&" "") "%" "")
      :outputs (str/split outputs #", ") }))

(def modules
  (reduce
    (fn [acc module] (assoc acc (get module :name) module))
    {}
    (map parse-module wires)))

(defn sim-flip [state outputs sender sig]
  "Send a signal to the flipflop, return [new-state pulses]."
  (if sig
    [state []]
    [(not state) (map (fn [o] [o (not state)]) outputs)]))

(defn sim-conj [state outputs sender sig]
  "Send a signal to the conjunction, return [new-state pulses]."
  (let
    [new-state (assoc state sender sig)
     all-active (reduce (fn [x y] (and x y)) (vals new-state))]
    [new-state (map (fn [o] [o (not all-active)]) outputs)]))

(defn sim-broadcast [state outputs sender sig]
  "Copy the signal to all outputs, return [new-state pulses]."
  [state (map (fn [o] [o sig]) outputs)])

(def defaults
  {"%" false
   "&" {}
   "i" false})

(def sims
  {"%" sim-flip
   "&" sim-conj
   "i" sim-broadcast})

(defn sim [state pending]
  (if (empty? pending)
    state
    (let
      [[src dst sig] (first pending)
       dst-mod (get modules dst {:kind "i" :outputs []})
       kind (get dst-mod :kind)
       outputs (get dst-mod :outputs)
       sim-module (get sims kind)
       default (get defaults kind)
       dst-state (get state dst default)
       [dst-new-state pulses] (sim-module dst-state outputs src sig)
       ; Prepend the sender to each of the pulses. The sender of the new
       ; pulses is the destination of the pulse we're currently processing.
       addr-pulses (map (fn [[p-dst p-sig]] [dst p-dst p-sig]) pulses)
       new-state (assoc state dst dst-new-state)]
      (println src "--[" sig "]-->" dst "::" pulses)
      ; (println "  " dst "state:" dst-state "->" dst-new-state)
      ; (println "   all states:" new-state)
      [new-state (into (vec (drop 1 pending)) addr-pulses)])))

(def sim-all
  (loop
    [state {}
     pending [["button" "broadcaster" false]]]
    (if (empty? pending)
      (println "Simulation is done")
      (let
        [[new-state new-pending] (sim state pending)]
        (recur new-state new-pending)))))

(defn run [opts]
  (println "Modules" modules)
  (println "Part 1:" "TODO"))
