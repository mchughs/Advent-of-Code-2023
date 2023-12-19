(time
 (let [[workflow-input parts-input] (map string/split-lines (string/split (slurp "src/day19/input.txt") #"\n\n"))
       workflow (into {} (map parse-workflow) workflow-input)
       parts (map zipmap (repeat '(:x :m :a :s)) (combo/combinations (range 1 100) 4))]
   (->> parts
        (keep (run workflow))
        count)))
