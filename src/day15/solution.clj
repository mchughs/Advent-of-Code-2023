(ns day15.solution
  (:require [clojure.string :as string]))

;; solution part 1

(defn run-hash [s]
  (reduce
   #(rem (* 17 (+ %1 (int %2))) 256)
   0
   s))

(time
 (let [input (string/split (string/trim (slurp "src/day15/input.txt")) #",")]
   (transduce
    (map run-hash)
    +
    0
    input)))

;; solution part 2

(defn parse [s]
  (let [[_ label focal-length] (re-matches #"(\w+)[\-\=](\d+)?" s)]
    (if focal-length
      {:label label
       :focal-length (Integer. focal-length)}
      {:label label})))

(defn install-lense [boxes {:keys [label focal-length]}]
  (let [box-idx (run-hash label)]
    (if focal-length
      (update boxes box-idx assoc label focal-length)
      (update boxes box-idx dissoc label))))

(defn calc-focusing-power [boxes]
  (reduce
   (fn [acc [box-idx box-contents]]
     (->> box-contents
          vals
          (map-indexed vector)
          (map (fn [[lense-idx focal-length]]
                 (* (inc box-idx) (inc lense-idx) focal-length)))
          (reduce + acc)))
   0
   boxes))

 ;; Leverages Clojure's array map which preserves insert order
(time
 (let [input (string/split (string/trim (slurp "src/day15/input.txt")) #",")
       setup (->> input
                  (map parse)
                  (reduce install-lense {}))]
   (calc-focusing-power setup)))
