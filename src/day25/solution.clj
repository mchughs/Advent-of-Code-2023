(ns day25.solution 
  (:require [clojure.set :as set]
            [clojure.string :as string]))

;; solution part 1

(defn parse [s]
  (re-seq #"\w+" s))

(defn- invert-map-of-sets [m]
  (reduce (fn [a [k v]]
            (assoc a k (conj (get a k #{}) v)))
          {}
          (for [[k s] m
                v s]
            [v k])))

(defn cut [graph [a b]]
  (-> graph (update a disj b) (update b disj a)))

(defn dfs
  [graph start]
  (loop [coll [start]
         visited #{}]
    (cond
      (empty? coll) visited
      (visited (peek coll)) (recur (pop coll) visited )
      :else (let [curr (peek coll)
                  node (graph curr)
                  coll (into (pop coll) node)
                  visited (conj visited curr)]
              (recur coll visited)))))

(time
 (let [input (->> "src/day25/input.txt" slurp string/split-lines (map parse))
       arbitrary-start (ffirst input)
       graph* (reduce (fn [m [node & remaining]] (assoc m node (set remaining))) {} input)
       graph** (invert-map-of-sets graph*)
       graph (merge-with set/union graph* graph**)
       nodes-count (count (keys graph))
       ;; Found by visually graphing my input data
       ;; I tried implementing Tarjan's algorithm to identify bridges but I couldn't get it to work.
       solution [["xzz" "kgl"]
                 ["vkd" "qfb"]
                 ["hqq" "xxq"]]
       split-graph (reduce cut graph solution)
       piece-one-size (count (dfs split-graph arbitrary-start))
       piece-two-size (- nodes-count piece-one-size)]
   (* piece-one-size piece-two-size)))
