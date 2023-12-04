(ns day04.solution
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

;; solution part 1

(defn- extract-input
  "\"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\"
   ->
   ['(41 48 83 86 17) '(83 86 6 31 17 9 48 53)]"
  [s]
  (let [[_ n winners numbers] (re-matches #"Card(?:\s)*(\d+):((?:\s)*(?:\d+(?:\s)*)*)\|((?:\s)*(?:\d+(?:\s)*)*)" s)]
    {:card (Integer. n)
     :winners (->> winners
                   (re-seq #"\d+")
                   (map #(Integer. %))
                   set)
     :numbers (->> numbers
                   (re-seq #"\d+")
                   (map #(Integer. %))
                   set)}))

(defn- score-card [{:keys [winners numbers]}]
  (reduce (fn [acc n]
            (if (contains? winners n)
              (if (zero? acc)
                1
                (* 2 acc))
              acc))
          0
          numbers))

(time
 (with-open [rdr (io/reader "src/day04/input.txt")]
   (->> rdr
        line-seq
        (transduce (comp (map extract-input)
                         (map score-card))
                   +
                   0))))

;; solution part 2

(defn- score-cards [cards unscored]
  (reduce (fn [acc {:keys [card winners numbers]}]
            (let [score (count (set/intersection winners numbers))
                  copies (range (inc card) (+ card score 1))]
              (into acc
                    (map #(get cards (dec %)))
                    copies)))
          '()
          unscored))

(time
 (with-open [rdr (io/reader "src/day04/input.txt")]
   (let [cards (->> rdr
                    line-seq
                    (mapv extract-input))]
     (loop [unscored cards
            scored '()]
       (if-not (empty? unscored)
         (recur (score-cards cards unscored) (into scored unscored))
         (count scored))))))
