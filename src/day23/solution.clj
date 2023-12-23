(ns day23.solution
  (:require [clojure.string :as string]))

;; --- A* algorithm code taken from https://matthewdowney.github.io/astar-in-clojure-find-k-shortest-paths.html

(declare a*, a*-seq, next-a*-path, unseen?, step-factory, rpath, cmp-step)

(defn a*
  "A sequence of paths from `src` to `dest`, shortest first, within the supplied `graph`.
  If the graph is weighted, supply a `distance` function. To make use of A*, supply a 
  heuristic function. Otherwise performs like Dijkstra's algorithm."
  [graph src dest & {:keys [distance heuristic]}]
  (let [init-adjacent (sorted-set-by cmp-step {:node src :cost 0 :entered 0})]
    (a*-seq graph dest init-adjacent
            (or distance (constantly 1))
            (or heuristic (constantly 0)))))

(defn a*-seq
  "Construct a lazy sequence of calls to `next-a*-path`, returning the shortest path first."
  [graph dest adjacent distance heuristic]
  (lazy-seq
   (when-let [[path, adjacent'] (next-a*-path graph dest adjacent distance heuristic)]
     (cons path (a*-seq graph dest adjacent' distance heuristic)))))

(defn next-a*-path [graph dest adjacent f-cost f-heur]
  (when-let [{:keys [node] :as current} (first adjacent)]
    (let [path (rpath current)
          adjacent' (disj adjacent current)] ;; "pop" the current node
      (if (= node dest)
        [(reverse path), adjacent']
        (let [last-idx (or (:entered (last adjacent')) 0)
              factory (step-factory current last-idx f-cost f-heur dest)
              xform (comp (filter (partial unseen? path)) (map-indexed factory))
              adjacent'' (into adjacent' xform (graph node))]
          (recur graph dest adjacent'' f-cost f-heur))))))

(defn unseen? [path node]
  (not-any? #{node} path))

(defn step-factory [parent last-insertion cost heur dest]
  (fn [insertion-idx node]
    {:parent parent
     :node node
     :entered (+ last-insertion (inc insertion-idx))
     :cost (+ (:cost parent) (cost (:node parent) node) (heur node dest))}))

(defn rpath [{:keys [node parent]}]
  (lazy-seq
   (cons node (when parent (rpath parent)))))

(defn cmp-step [step-a step-b]
  (let [cmp (compare (:cost step-a) (:cost step-b))]
    (if (zero? cmp)
      (compare (:entered step-a) (:entered step-b))
      cmp)))

;; solution part 1

(defn return-neighbors-fn [tiles]
  (fn [x y tile]
    (let [above [x (dec y)]
          above' (let [t (get tiles above)]
                   (when-not (or (= \v t)
                                 (nil? t))
                     above))
          left [(dec x) y]
          left' (let [t (get tiles left)]
                  (when-not (or (= \> t)
                                (nil? t))
                    left))
          down [x (inc y)]
          down' (when-not (nil? (get tiles down)) down)
          right [(inc x) y]
          right' (when-not (nil? (get tiles right))
                  right)]
      (->> (case tile
             \v [down']
             \> [right']
             \. [above' left' right' down'])
           (remove nil?)
           set))))

(defn get-graph [tiles]
  (let [accessible (into {} (remove (fn [[_ v]] (= \# v))) tiles)
        return-neighbors (return-neighbors-fn accessible)]
    (->> accessible
         (map (fn [[[x y] tile]]
                [[x y] (return-neighbors x y tile)]))
         (remove (fn [[_ v]] (empty? v)))
         (into {}))))

;; ~ 40s
(time
 (let [input (string/split-lines (slurp "src/day23/input.txt"))
       [max-x max-y] ((juxt (comp count first) count) input)
       tiles (into {}
                   (for [x (range max-x)
                         y (range max-y)]
                     [[x y] (get-in input [y x])]))
       start [1 0]
       end [(- max-x 2) (dec max-y)]
       graph (get-graph tiles)]
   (->> (a* graph start end)
        last
        count 
        dec)))

;; solution part 2
