;; Execute with `clj -X main/run` from the parent directory.
(ns main
  (:require [clojure.data.priority-map :as priority-map])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def field
  "The input map/field that we are pathfinding on."
  (str/split-lines (slurp "example.txt")))

(def field-w (count (get field 0)))
(def field-h (count field))

(defn at-exit? [node]
  "Test whether this node is the exit."
  (and (= (get node :x) (- field-w 1)
       (= (get node :y) (- field-h 1)))))

(defn in-bounds? [node]
  "Test whether this node lies on the map."
  (and (>= (get node :x) 0)
       (>= (get node :y) 0)
       (< (get node :x) field-w)
       (< (get node :y) field-h)))

(defn heat-loss [x y]
  (let [ch (get (get field y) x)] (Character/digit ch 10)))

(def start-nodes
  "We start in the top-left, moving east or south, with 3 steps before a forced turn."
  (priority-map/priority-map
    {:x 0 :y 0 :dir :east :budget 3} 0
    {:x 0 :y 0 :dir :south :budget 3} 0))

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
     :y (cond (= :north dir) (dec y) (= :south dir) (inc x) :else y)
     :dir dir
     ; If we took a step in a different direction than we were going, the budget
     ; resets to 3 and we use 1, so we have 2 steps remaining. If we stepped in
     ; the same direction, just decrement the budget.
     :budget (if (= dir (get node :dir)) (dec (get node :budget)) 2)}))

(defn steps [node]
  "Return all steps that we can take from the current node."
  (let
    [dir (get node :dir)
     new-dirs [conj (turns dir) dir]
     new-nodes (map (fn [d] (step d node)) new-dirs)]
    (filter (fn [n] (>= (get n :budget) 0)) new-nodes)))

(defn find-route-rec [open closed]
  (let [[node cost] (peek open)
        new-open (pop open)
        new-closed (conj closed node)]
    (cond
      ; If we are at the exit, the final result is the cost it took to reach it.
      (at-exit? node) cost
      ; If the node is out of bounds, just drop it and continue.
      (not (in-bounds? node)) (find-route-rec new-open closed)
      ; Same if we visited the node before.
      (contains? closed node) (find-route-rec new-open closed)
      :else
        (let [new-nodes (steps node)
              add-opens (map (fn [n] [n (inc cost)]) new-nodes)
              new-open' (apply conj new-open add-opens)]
          (find-route-rec new-open' new-closed)))))

(defn run [opts]
  (println "The input is:")
  (println field)
  (println "The field size is:" field-w field-h)
  (println "At 5,5 we have:" (heat-loss 5 5)))
