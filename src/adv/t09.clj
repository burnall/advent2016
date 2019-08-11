(ns adv.t09
  (:require [adv.util :refer [parse-int]]))

(def input 
  (->> "data/t09.txt"
       (slurp)
       (clojure.string/trim-newline)))

(defn get-markers [s]
  (let [matcher (re-matcher #"\((\d+)x(\d+)\)" s)]
    (->> {:markers []}
         (iterate (fn [{markers :markers}]
                    (if-let [[_ a b] (re-find matcher)]
                      {:markers (conj markers 
                                      {:a (parse-int a)
                                       :b (parse-int b)
                                       :start (.start matcher)
                                       :end (.end matcher)})}
                      {:end markers})))                  
         (some :end))))        

(defn prune-markers [markers]
  (->> markers
       (reduce (fn [{:keys [markers prev-end]} marker] 
                 (if (>= (:start marker) prev-end)
                   {:markers (conj markers marker), :prev-end (+ (:end marker) (:a marker))}
                   {:markers markers, :prev-end prev-end}))
               {:markers [], :prev-end -1})    
       :markers))            

(defn solve 
  ([s] 
    (->> s
         (get-markers)
         (prune-markers)
         (map (fn [{:keys [a b start end]}] (- (* a (dec b)) (- end start))))
         (reduce +)
         (+ (count s))))

  ([] (solve input)))
