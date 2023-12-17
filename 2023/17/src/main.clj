;; Execute with `clj -X main/run` from the parent directory.
(ns main
  (:require [clojure.data.priority-map :as priority-map])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def field
  "The input map/field that we are pathfinding on."
  (str/split-lines (slurp "input.txt")))

(def field-w (count (get field 0)))
(def field-h (count field))

(defn at-exit? [node]
  "Test whether this node is the exit."
  (and (= (get node :x) (dec field-w))
       (= (get node :y) (dec field-h))))

(defn in-bounds? [node]
  "Test whether this node lies on the map."
  (and (>= (get node :x) 0)
       (>= (get node :y) 0)
       (< (get node :x) field-w)
       (< (get node :y) field-h)))

(defn heat-loss [node]
  (let [x (get node :x)
        y (get node :y)
        ch (get (get field y) x)]
    (Character/digit ch 10)))

(def crucible-part1 {:min-ahead 0 :max-ahead 3})
(def crucible-part2 {:min-ahead 4 :max-ahead 10})

(defn start-nodes [crucible]
  "We start in the top-left, moving east or south, with 3 steps before a forced turn."
  (priority-map/priority-map
    {:x 0
     :y 0
     :dir :east
     :max-ahead (get crucible :max-ahead)
     :min-ahead (get crucible :min-ahead)
     :is-valid? true} 0
    {:x 0
     :y 0
     :dir :south
     :max-ahead (get crucible :max-ahead)
     :min-ahead (get crucible :min-ahead)
     :is-valid? true} 0))

(defn turns [dir]
  "Return the two directions in which we can turn from `dir`."
  (cond
    (= :north dir) #{:west :east}
    (= :south dir) #{:west :east}
    (= :west dir) #{:north :south}
    (= :east dir) #{:north :south}))

(defn step [crucible dir node]
  "Adjust :x and :y to take a step in the given direction."
  (let
    [x (get node :x)
     y (get node :y)
     ahead (= dir (get node :dir))
     can-ahead (> (get node :max-ahead) 0)
     can-turn (<= (get node :min-ahead) 0)]
    {:x (cond (= :east dir) (inc x) (= :west dir) (dec x) :else x)
     :y (cond (= :north dir) (dec y) (= :south dir) (inc y) :else y)
     :dir dir
     :min-ahead (dec (get (if ahead node crucible) :min-ahead))
     :max-ahead (dec (get (if ahead node crucible) :max-ahead))
     :is-valid? (if ahead can-ahead can-turn)}))

(defn steps [crucible node]
  "Return all steps that we can take from the current node."
  (let
    [dir (get node :dir)
     new-dirs (conj (turns dir) dir)
     new-nodes (map (fn [d] (step crucible d node)) new-dirs)]
    (filter (fn [n] (and (get n :is-valid?) (in-bounds? n))) new-nodes)))

; If we use conj to insert into the priority queue, we might overwrite the
; cost for a given node with a larger cost. This keeps the node with the lowest
; cost instead.
(defn conj-best [opens [node prio]]
  (cond
    (not (contains? opens node)) (conj opens [node prio])
    (< (get opens node) prio) opens
    :else (conj opens [node prio])))

(defn best-route-cost [crucible]
  (loop
    [open (start-nodes crucible)
     closed #{}]
    (let [[node cost] (peek open)
          new-open (pop open)
          new-closed (conj closed node)]
      (cond
        ; If we are at the exit, the final result is the cost it took to reach it.
        (at-exit? node) cost
        ; Skip nodes that we visited before.
        (contains? closed node) (recur new-open closed)
        :else
          (let [new-nodes (steps crucible node)
                add-opens (map (fn [n] [n (+ cost (heat-loss n))]) new-nodes)
                new-open' (reduce conj-best new-open add-opens)]
            (recur new-open' new-closed))))))

(defn run [opts]
  (println "Part 1:" (best-route-cost crucible-part1))
  (println "Part 2:" (best-route-cost crucible-part2)))
