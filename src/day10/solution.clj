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
 (def answer
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
         {:visited (assoc visited a i) :distance i}
         (let [step (step-fn {:tiles tiles
                              :visited (set (keys visited))})
               [a'] (step a)
               [b'] (step b)]
           (recur (inc i) (assoc visited a i b i) [a' b'])))))))

(:distance answer)

;; solution part 2

(def curve
  (set (keys (:visited answer))))

(defn interior-points [curve [y line]]
  (->> line
       (map-indexed vector)
       (reduce
        (fn [{:keys [in? cnt run-started?] :as acc} [x char]]
          (let [on-curve? (contains? curve [x y])]
            (cond
              (and on-curve? (= \| char))
              {:in? (not in?) :cnt cnt :run-started? false}

              (and on-curve? (#{\S \L \7 \F \J} char) (not run-started?))
              {:in? (not in?) :cnt cnt :run-started? char}

              (and on-curve?
                   (= \L run-started?)
                   (= \7 char))
              {:in? in? :cnt cnt :run-started? false}

              (and on-curve?
                   (= \F run-started?)
                   (= \J char))
              {:in? in? :cnt cnt :run-started? false}

              (and on-curve?
                   (= \L run-started?)
                   (= \J char))
              {:in? (not in?) :cnt cnt :run-started? false}

              (and on-curve?
                   (= \F run-started?)
                   (= \7 char))
              {:in? (not in?) :cnt cnt :run-started? false}

              (and (not on-curve?) in?)
              {:in? in? :cnt (inc cnt) :run-started? false}

              :else
              acc)))
        {:cnt 0 :in? false :run-started? false})
       :cnt))

(time
 (let [replacement "7" ;; depends on the input. too lazy to do it programmatically and it's easy to do by hand.
       input (string/split-lines (string/replace (slurp "src/day10/input.txt") #"S" replacement))
       extra-line (apply str (repeat (+ 2 (count (first input))) "."))
       padded-input (concat [extra-line] (map #(str "." % ".") input) [extra-line])
       padded-curve (into #{} (map (fn [[x y]] [(inc x) (inc y)])) curve)]
   (->> padded-input
        (map-indexed vector)
        (map (partial interior-points padded-curve))
        (apply +))))
 