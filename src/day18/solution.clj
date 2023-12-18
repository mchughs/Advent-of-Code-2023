(ns day01.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def ^:private digits
  #{\1 \2 \3 \4 \5 \6 \7 \8 \9})

;; solution part 1

(defn- extract-calibration-value [s]
  (Integer. (str (some digits s)
                 (some digits (reverse s)))))

(time
 (with-open [rdr (io/reader "src/day01/input.txt")]
   (->> rdr
        line-seq
        (transduce (map extract-calibration-value)
                   +
                   0))))

;; solution part 2

(def ^:private digits->representations
  {1 #{"1" "one" "eno"}
   2 #{"2" "two" "owt"}
   3 #{"3" "three" "eerht"}
   4 #{"4" "four" "ruof"}
   5 #{"5" "five" "evif"}
   6 #{"6" "six" "xis"}
   7 #{"7" "seven" "neves"}
   8 #{"8" "eight" "thgie"}
   9 #{"9" "nine" "enin"}})

(defn- invert-map-of-sets [m]
  (reduce (fn [a [k v]] (assoc a k v))
          {}
          (for [[k s] m
                v s]
            [v k])))

(def ^:private representations->digits
  (invert-map-of-sets digits->representations))

(def ^:private match-string
  (string/join "|" (map #(string/join "|" %) (vals digits->representations))))

(defn- find-first-digit [s]
  (representations->digits (re-find (re-pattern match-string) s)))

(defn- extract-calibration-value+ [s]
  (Integer. (str (find-first-digit s)
                 (find-first-digit (apply str (reverse s))))))

(time
 (with-open [rdr (io/reader "src/day01/input.txt")]
   (->> rdr
        line-seq
        (transduce (map extract-calibration-value+)
                   +
                   0))))
