(ns day07.solution
  (:require [clojure.string :as string]))

(def card->value
  {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9
   \T 10 \J 11 \Q 12 \K 13 \A 14})

(def type->value
  {{5 1} 6 ;; five of a kind
   {4 1} 5 ;; four of a kind
   {2 1, 3 1} 4 ;; full house
   {3 1} 3 ;; three of a kind
   {2 2} 2 ;; two pair
   {2 1} 1 ;; one pair
   {} 0}) ;; high card

;; solution part 1

(time
 (let [input (->> (slurp "src/day07/input.txt")
                  string/split-lines
                  (map #(string/split % #" ")))
       hands (reduce (fn [acc [hand bid]]
                       (let [hand (map card->value hand)]
                         (conj acc {:hand hand
                                    :bid (Integer. bid)
                                    :type (-> hand
                                              frequencies
                                              vals
                                              frequencies
                                              (dissoc 1))})))
                     []
                     input)
       rank (->> hands
                 (sort-by (juxt (comp type->value :type)
                                (comp #(nth % 0) :hand)
                                (comp #(nth % 1) :hand)
                                (comp #(nth % 2) :hand)
                                (comp #(nth % 3) :hand)
                                (comp #(nth % 4) :hand))))]
   (->> rank
        (map-indexed (fn [idx {bid :bid}]
                       (* bid (inc idx))))
        (apply +))))

;; solution part 2

;; AAAAA 0
;; AA8AA 0
;; 23332 0
;; TTT98 0
;; 23432 0
;; A23A4 0
;; 23456 0

