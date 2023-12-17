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

(def start-nodes
  "We start in the top-left, moving east or south, with 3 steps before a forced turn."
  (priority-map/priority-map
    {:x 0 :y 0 :dir :east :budget 3 :route []} 0
    {:x 0 :y 0 :dir :south :budget 3 :route []} 0))

(defn turns [dir]
  "Return the two directions in which we can turn from `dir`."
  (cond
    (= :north dir) #{:west :east}
    (= :south dir) #{:west :east}
    (= :west dir) #{:north :south}
    (= :east dir) #{:north :south}))

(defn step [dir node]
  "Adjust :x and :y to take a step in the given direction."
  (let [x (get node :x) y (get node :y)]
    {:x (cond (= :east dir) (inc x) (= :west dir) (dec x) :else x)
     :y (cond (= :north dir) (dec y) (= :south dir) (inc y) :else y)
     :dir dir
     ; If we took a step in a different direction than we were going, the budget
     ; resets to 3 and we use 1, so we have 2 steps remaining. If we stepped in
     ; the same direction, just decrement the budget.
     :budget (if (= dir (get node :dir)) (dec (get node :budget)) 2)
     :route (cons {:x x :y y} (get node :route))}))

(defn steps [node]
  "Return all steps that we can take from the current node."
  (let
    [dir (get node :dir)
     new-dirs (conj (turns dir) dir)
     new-nodes (map (fn [d] (step d node)) new-dirs)]
    (filter
      (fn [n] (and
                (>= (get n :budget) 0)
                (in-bounds? n)))
      new-nodes)))

(def best-route-cost
  (loop
    [open start-nodes
     closed #{}]
    (let [[node cost] (peek open)
          new-open (pop open)
          new-closed (conj closed (dissoc node :route))]
      (cond
        ; If we are at the exit, the final result is the cost it took to reach it.
        (at-exit? node) cost
        ; Skip nodes that we visited before.
        (contains? closed (dissoc node :route)) (recur new-open closed)
        :else
          (let [new-nodes (steps node)
                add-opens (map (fn [n] [n (+ cost (heat-loss n))]) new-nodes)
                new-open' (apply conj new-open add-opens)]
            (recur new-open' new-closed))))))

(defn run [opts]
  (println "Part 1:" best-route-cost))
