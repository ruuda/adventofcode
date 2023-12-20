;; Execute with `clj -X main/run` from the parent directory.
(ns main
  (:require [clojure.core.reducers :as r])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def wires
  "The wires that connect the different components."
  (str/split-lines (slurp "example1.txt")))

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
    (fn [acc module] (conj acc {(get module :name) module}))
    {}
    (map parse-module wires)))

(defn sim-flip [state outputs sender sig]
  "Send a signal to the flipflop, return [new-state pulses]."
  (if sig
    [(not state) (map (fn [o] [o (not state)]) outputs)]
    [state []]))

(defn sim-conj [state outputs sender sig]
  "Send a signal to the conjunction, return [new-state pulses]."
  (let
    [new-state (conj state {sender sig})
     all-active (and (vals new-state))]
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
    (let [[src dst sig] (peek pending)
          pending' (pop pending)
          dst-mod (get modules dst)
          kind (get dst-mod :kind)
          outputs (get dst-mod :outputs)
          sim-module (get sims kind)
          default (get defaults kind)
          dst-state (get dst state default)
          [dst-new-state pulses] (sim-module dst-state outputs src sig)
          ; Prepend the sender to each of the pulses. The sender of the new
          ; pulses is the destination of the pulse we're currently processing.
          addr-pulses (map (fn [[p-dst p-sig]] [dst p-dst p-sig]) pulses)
          new-state (assoc state dst dst-new-state)]
      (println src "--[" sig "]-->" dst "==>" pulses)
      [new-state (concat (pop pending) addr-pulses)])))

(defn run [opts]
  (println "Modules" modules)
  (println (sim {} [["button" "broadcaster" false]]))
  (println "Part 1:" "TODO"))
