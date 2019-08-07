(ns adv.t07
  (:require [adv.util :refer [split-lines split]]))

(def input 
  (->> "data/t07.txt"
       (slurp)
       (split-lines)))

(defn abba? [s]
  (->> s
       (re-seq #"(.)(.)\2\1")
       (filter (fn [[_ a b]] (not= a b)))
       (seq)
       (boolean)))

(defn fit? [s]
  (let [xs (split s #"\[|\]")]
    (and (some abba? (take-nth 2 xs))
         (not-any? abba? (take-nth 2 (rest xs)))))) 

(defn solve
  ([lines] 
    (->> lines
         (filter fit?)
         (count)))
  ([] (solve input)))

