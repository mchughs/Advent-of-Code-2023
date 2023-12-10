(ns day10.solution
  (:require [clojure.string :as string]))

;; solution part 1

(def directions
  {:north #(update % 1 dec)
   :west #(update % 0 dec)
   :south #(update % 1 inc)
   :east #(update % 0 inc)})

(def connections
  {:north #{\| \7 \F}
   :west #{\- \L \F}
   :south #{\| \L \J}
   :east #{\- \J \7}})

(def accessible-directions
  {\S [:north :west :south :east]
   \| [:north :south]
   \- [:west :east]
   \L [:north :east]
   \7 [:west :south]
   \F [:south :east]
   \J [:north :west]})

(defn step-fn [{:keys [tiles visited]}]
  (fn [point]
    (->> (get tiles point)
         accessible-directions
         (map (fn [direction]
                (let [f (get directions direction)
                      point' (f point)
                      tile (get tiles point')
                      connected? #((get connections direction) %)]
                  (and
                   (not (visited point'))
                   (connected? tile)
                   point'))))
         (filter identity))))

(time
 (let [input (string/split-lines (slurp "src/day10/input.txt"))
       [max-x max-y] ((juxt (comp count first) count) input)
       tiles (into {}
                   (for [x (range max-x)
                         y (range max-y)]
                     [[x y] (get-in input [y x])]))
       start (ffirst (filter (fn [[_ v]] (= \S v)) tiles))
       ;; the first step is an exception because there are two legal moves.
       ;; every other step only has one legal move until reaching the end.
       [a b] ((step-fn {:tiles tiles
                        :visited #{}})
              start)]
   (loop [i 1
          visited {start 0
                   a 1
                   b 1}
          [a b] [a b]]
     (if (= a b)
       i
       (let [step (step-fn {:tiles tiles
                            :visited (set (keys visited))})
             [a'] (step a)
             [b'] (step b)]
         (recur (inc i) (assoc visited a i b i) [a' b']))))))

;; solution part 2
