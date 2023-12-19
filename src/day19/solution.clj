 (ns day19.solution
  (:require [clojure.string :as string]))

 ;; solution part 1

(defmacro build-fn [f variable op n target]
  `(fn [part#]
     (let [value# (get part# (keyword ~variable))
           op# (resolve (read-string ~op))
           n# (Integer. ~n)]
       (if (op# value# n#)
         ~target
         (~f part#)))))

(defn parse-workflow [s]
  (let [[_ label exprs default] (re-matches #"(\w+)\{(.*?),([RA]|\w+)\}" s)
        expressions (reverse (map rest (re-seq #"([xmas])([><])(\d+)\:([RA]|\w+)" exprs)))]
    [label (reduce (fn [f [variable op n target]]
                     (build-fn f variable op n target))
                   (constantly default)
                   expressions)]))

(defn parse-part [z]
  (let [[x m a s] (map #(Integer. %) (re-seq #"\d+" z))]
    {:x x :m m :a a :s s}))

(defn run [workflow]
  (fn [part]
    (loop [curr "in"]
      (case curr
        "A" part
        "R" nil
        (recur ((get workflow curr) part))))))

;; ~ 20ms
(time
 (let [[workflow-input parts-input] (map string/split-lines (string/split (slurp "src/day19/input.txt") #"\n\n"))
       workflow (into {} (map parse-workflow) workflow-input)
       parts (map parse-part parts-input)]
   (->> parts
        (keep (run workflow))
        (map vals)
        flatten
        (apply +))))

 ;; solution part 2
