(ns day02.solution
  (:require [clojure.java.io :as io]))

(def ^:private allowed-cubes
  {:red 12
   :green 13
   :blue 14})

;; solution part 1

(defn- possible-game-id [s]
  (let [id (Integer. (last (re-find #"Game (\d+)" s)))
        possible? (->> s
                       (re-seq #"(\d+) (blue|green|red)")
                       (map (fn [[_ digit color]]
                              [(keyword color)
                               (Integer. digit)]))
                       (every? (fn [[k v]]
                                 (<= v (k allowed-cubes)))))]
    (if possible?
      id
      0)))

(time
 (with-open [rdr (io/reader "src/day02/input.txt")]
   (->> rdr
        line-seq
        (transduce (map possible-game-id)
                   +
                   0))))

;; solution part 2

(defn- game-powers [s]
  (->> s
       (re-seq #"(\d+) (blue|green|red)")
       (map (fn [[_ digit color]]
              [(keyword color)
               (Integer. digit)]))
       (reduce (fn [acc [k v]]
                 (let [curr-v (get acc k 0)]
                   (if (< curr-v v)
                     (assoc acc k v)
                     acc)))
               {})
       vals
       (apply *)))

(time
 (with-open [rdr (io/reader "src/day02/input.txt")]
   (->> rdr
        line-seq
        (transduce (map game-powers)
                   +
                   0))))
