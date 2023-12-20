;; Execute with `clj -X main/run` from the parent directory.
(ns main
  (:require [clojure.core.reducers :as r])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def wires
  "The wires that connect the different components."
  (str/split-lines (slurp "input.txt")))

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

(def initial-states
  (reduce
    (fn [states module]
      (let
        [src (get module :name)]
        (reduce
         (fn [st dst] (assoc st dst (assoc (get st dst {}) src false)))
         states
         (get module :outputs))))
    {}
    (vals modules)))

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

(defn default-state [mod-name]
  {"%" false
   "&" (get initial-states mod-name)
   "i" false})

(def sims
  {"%" sim-flip
   "&" sim-conj
   "i" sim-broadcast})

(defn sim [state pending]
  "Simulate a single pending signal, return the next state and pending signals."
  (let
    [[src dst sig] (first pending)
     dst-mod (get modules dst {:kind "i" :outputs []})
     kind (get dst-mod :kind)
     outputs (get dst-mod :outputs)
     sim-module (get sims kind)
     default (get (default-state dst) kind)
     dst-state (get state dst default)
     [dst-new-state pulses] (sim-module dst-state outputs src sig)
     ; Prepend the sender to each of the pulses. The sender of the new
     ; pulses is the destination of the pulse we're currently processing.
     addr-pulses (map (fn [[p-dst p-sig]] [dst p-dst p-sig]) pulses)
     new-state (assoc state dst dst-new-state)]
    (println src "--[" sig "]-->" dst "::" pulses)
    ; (println "  " dst "state:" dst-state "->" dst-new-state)
    ; (println "   all states:" new-state)
    [new-state (into (vec (drop 1 pending)) addr-pulses)]))

(defn sim-press-once [state pulse-count]
  "Simulate one button press until pulses quiesce, return [new-state pulse-count]."
  (loop
    [state state
     pending [["button" "broadcaster" false]]
     pulse-count pulse-count]
    (if (empty? pending)
      [state pulse-count]
      (let
        [[_src _dst sig] (first pending)
         [new-state new-pending] (sim state pending)
         new-pulse-count (assoc pulse-count sig (inc (get pulse-count sig)))]
        (recur new-state new-pending new-pulse-count)))))

(def sim-press-1000
  (loop
    [n-left 1000
     state {}
     pulse-count {false 0 true 0}]
    (if (= 0 n-left)
      pulse-count
      (let
        [[new-state new-pulse-count] (sim-press-once state pulse-count)]
        (println "Iteration" (- 1001 n-left))
        (recur (dec n-left) new-state new-pulse-count)))))

(defn run [opts]
  (println "Modules" modules)
  (println "Initial states" initial-states)
  (println "Part 1:" (apply * (vals sim-press-1000))))

; For part 2, probably this input is simulating an n-bit adder or something and
; it will take something like 2^{large-ish} button presses to count up and send
; a low pulse to `rx`. Instead of going forward and seeing when it happens,
; probably we need to work backwards and do it symbolically again.
