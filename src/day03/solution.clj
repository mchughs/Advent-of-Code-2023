(ns day03.solution
  (:require [clojure.string :as string]))

;; solution part 1

(defn- re-seq-pos [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
       (when (. m find)
         (cons {:start-x (. m start) :end-x (. m end) :n (Integer. (. m group))}
               (lazy-seq (step))))))))

(defn- extract-numbers [y s]
  (->> s
       (re-seq-pos #"\d+")
       (map #(assoc % :y y))))

(defn- neighbors [{:keys [start-x end-x y n]} [max-x max-y]]
  (let [top (for [x (range (dec start-x) (inc end-x))
                  :let [y' (dec y)]
                  :when (and (<= 0 x max-x)
                             (<= 0 y'))]
              [x y'])
        edges (for [x [(dec start-x) end-x]
                    :when (<= 0 x max-x)]
                [x y])
        bottom (for [x (range (dec start-x) (inc end-x))
                     :let [y' (inc y)]
                     :when (and (<= 0 x max-x)
                                (<= y' max-y))]
                 [x y'])]
    [n (concat top edges bottom)]))

(defn- sym? [schematic [x y]]
  (nil? (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \.}
         (get-in schematic [y x]))))

(time
 (let [input (string/split-lines (slurp "src/day03/input.txt"))
       [max-x max-y] ((juxt (comp dec count first) (comp dec count)) input)
       candidates (apply concat (map-indexed extract-numbers input))]
   (transduce (comp
               (map #(neighbors % [max-x max-y]))
               (filter (fn [[_ points]]
                         (some #(sym? input %) points)))
               (map first))
              +
              0
              candidates)))

;; solution part 2

(defn- get-gears [schematic [max-x max-y]]
  (for [x (range 0 (inc max-x))
        y (range 0 (inc max-y))
        :when (= \* (get-in schematic [y x]))]
    [x y]))

(defn- invert-map-of-sets [m]
  (reduce (fn [a [k v]]
            (assoc a k (conj (get a k #{}) v)))
          {}
          (for [[k s] m
                v s]
            [v k])))

(time
 (let [input (string/split-lines (slurp "src/day03/input.txt"))
       [max-x max-y] ((juxt (comp dec count first) (comp dec count)) input)
       candidates (->> input
                       (map-indexed extract-numbers)
                       (apply concat)
                       (map #(neighbors % [max-x max-y]))
                       (reduce (fn [m [k v]]
                                 (update m k #(if (seq %) ;; some numbers appear multiple times in the schematic
                                                (into % v)
                                                (set v))))
                               {})
                       invert-map-of-sets)
       gears (get-gears input [max-x max-y])]
   (transduce (comp
               (map #(get candidates %))
               (filter #(= 2 (count %)))
               (map #(apply * %)))
              +
              0
              gears)))
