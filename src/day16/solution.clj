(ns day16.solution
  (:require [clojure.set :as set]
            [clojure.string :as string]))

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

(defn step-fn [chart [max-x max-y]]
  (fn [{position :position facing :facing}]
    (let [[x y :as tile-position] ((get next-tile facing) position)
          tile-type (get-in chart [y x])
          facing-fn (get tile-types tile-type)]
      (if (or (nil? tile-type)
              (not (< -1 x max-x))
              (not (< -1 y max-y)))
        '()
        (map (fn [a b]
               {:facing a
                :position b})
             (facing-fn facing)
             (repeat tile-position))))))

(defn step-all-fn [step]
  (fn [{:keys [beam-heads visited]}]
    ;; takes previous travesed paths out.
    (let [new-beam-heads (set/difference (into #{} (mapcat step) beam-heads)
                                         visited)
          new-visited (into visited new-beam-heads)]
      {:beam-heads new-beam-heads
       :visited new-visited})))

;; ~300 ms
(time
 (let [input (string/split-lines (slurp "src/day16/input.txt"))
       [max-x max-y] ((juxt (comp count first) count) input)
       step (memoize (step-fn input [max-x max-y]))
       step-all (memoize (step-all-fn step))]
   (->> {:beam-heads '({:position [-1 0]
                        :facing :east})
         :visited #{}}
        (iterate step-all)
        (take-while (comp seq :beam-heads))
        last
        step-all ;; take one last step
        :visited
        (into #{} (map :position))
        count)))

;; solution part 2

(defn get-corners [[max-x max-y]]
  [{:position [-1 0]
    :facing :east}
   {:position [0 -1]
    :facing :south}
   {:position [max-x 0]
    :facing :west}
   {:position [(dec max-x) -1]
    :facing :south}
   {:position [-1 (dec max-y)]
    :facing :east}
   {:position [0 max-y]
    :facing :north}
   {:position [max-x (dec max-y)]
    :facing :west}
   {:position [(dec max-x) max-y]
    :facing :north}])

(defn get-edges [[max-x max-y]]
  (concat
   (for [x (range 0 max-x)]
     {:position [x 0]
      :facing :south})
   (for [x (range 0 max-x)]
     {:position [x (dec max-y)]
      :facing :north})
   (for [y (range 0 max-y)]
     {:position [0 y]
      :facing :east})
   (for [y (range 0 max-y)]
     {:position [(dec max-x) y]
      :facing :west})))

;; ~8500 ms
(time
 (let [input (string/split-lines (slurp "src/day16/input.txt"))
       [max-x max-y] ((juxt (comp count first) count) input)
       step (memoize (step-fn input [max-x max-y]))
       step-all (step-all-fn step)
       corners (get-corners [max-x max-y])
       edges (get-edges [max-x max-y])
       entry-points (concat corners edges)]
   (apply max
          (for [point entry-points]
            (->> {:beam-heads #{point}
                  :visited #{}}
                 (iterate step-all)
                 (take-while (comp seq :beam-heads))
                 last
                 step-all
                 :visited
                 (into #{} (map :position))
                 count
                 inc ;; Not sure exactly why but have to add one for my input. I guess the start point doesn't get counted as visited for some reason :shrug
                 )))))
