(ns day16.solution
  (:require [clojure.string :as string]))

;; solution part 1

(def tile-types
  {\. vector
   \- {:north [:west :east]
       :south [:west :east]
       :east [:east]
       :west [:west]}
   \| {:north [:north]
       :south [:south]
       :east [:north :south]
       :west [:north :south]}
   \\ {:north [:west]
       :south [:east]
       :east [:south]
       :west [:north]}
   \/ {:north [:east]
       :south [:west]
       :east [:north]
       :west [:south]}})

(def next-tile
  {:north (fn [[x y]] [x (dec y)])
   :south (fn [[x y]] [x (inc y)])
   :east (fn [[x y]] [(inc x) y])
   :west (fn [[x y]] [(dec x) y])})

(defn step [chart {position :position facing :facing}]
  (let [[x y :as tile-position] ((get next-tile facing) position)
        tile-type (get-in chart [y x])
        facing-fn (get tile-types tile-type)]
    (if (nil? tile-type)
      []
      (mapv (fn [a b]
              {:facing a
               :position b})
            (facing-fn facing)
            (repeat tile-position)))))

(defn step-all-fn [input [max-x max-y]]
  (fn [{:keys [beam-heads visited]}]
    (let [new-beam-heads (->> beam-heads
                              (mapcat #(step input %))
                              (filter (fn [{[x y] :position}]
                                        (and (< -1 x max-x)
                                             (< -1 y max-y)))))
          new-visited (into visited
                            (map :position)
                            new-beam-heads)]
      {:beam-heads new-beam-heads
       :visited new-visited})))

(time
 (let [input (string/split-lines (slurp "src/day16/input.txt"))
       [max-x max-y] ((juxt (comp count first) count) input)
       step-all (step-all-fn input [max-x max-y])]
   (->> {:beam-heads '({:position [-1 0]
                        :facing :east})
         :visited #{}}
        (iterate step-all)
        (map (comp count :visited))
        ;; 10 is a heuristic for the number of repetitions of visited amounts to expect.
        ;; the beams can repeat the amount of places they visited a few time in a row.
        ;; to improve this we should also track the facing direction on each visited point.
        ;; then we could probably lower this down to 2 instead of 10.
        (partition 10 1)
        (take-while #(apply not= %))
        last
        last)))

;; solution part 2
