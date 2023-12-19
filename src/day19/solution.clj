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

(defn parse-workflow* [s]
  (let [[_ label exprs default] (re-matches #"(\w+)\{(.*?),([RA]|\w+)\}" s)
        expressions (map rest (re-seq #"([xmas])([><])(\d+)\:([RA]|\w+)" exprs))
        len (count expressions)]
    (into {}
          (map-indexed (fn [idx [variable op n target]]
                         [(str label idx)
                          [variable op n (or (#{"A" "R"} target)
                                             (str target 0))
                           (let [nxt-idx (inc idx)]
                             (if (< nxt-idx len)
                               (str label nxt-idx)
                               (or (#{"A" "R"} default)
                                   (str default 0))))]]))
          expressions)))

(defmacro build-splitter [variable op n]
  `(fn [parts#]
     (let [k# (keyword ~variable)
           n# (Integer. ~n)]
       (if (= "<" ~op)
         [(assoc-in parts# [k# 1] (dec n#)) (assoc-in parts# [k# 0] n#)]
         [(assoc-in parts# [k# 0] (inc n#)) (assoc-in parts# [k# 1] n#)]))))

(defn build-tree [m k]
  (if-let [[variable op n left right] (get m k)]
    {:label k
     :condition (build-splitter variable op n)
     :children [(build-tree m left) (build-tree m right)]}
    k))

(defn count-combos
  "(count-combos {:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]})
   => 256000000000000"
  [parts]
  (reduce-kv (fn [acc _ [a b]]
               (* acc (inc (- b a))))
             1
             parts))

(defn count-node [{:keys [condition children] :as node} parts]
  (case node
    "R" 0
    "A" (count-combos parts)
    (let [[l-child r-child] children
          [l-parts r-parts] (condition parts)]
      (+ (count-node l-child l-parts)
         (count-node r-child r-parts)))))

;; ~20ms
(time
 (let [[workflow-input _] (map string/split-lines (string/split (slurp "src/day19/input.txt") #"\n\n"))
       workflow (into {} (map parse-workflow*) workflow-input)
       root "in0"
       init {:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]}
       tree (build-tree workflow root)]
   (count-node tree init)))
