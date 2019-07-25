(ns adv.t02
  (:require [adv.util :refer [split-lines]]))

(def input 
  (->> "data/t02.txt"
       (slurp)
       (split-lines)))

(def field 
 [[1 2 3]
  [4 5 6]
  [7 8 9]])

(def gradients 
  {\L [0 -1]
   \U [-1 0]
   \R [0 1]
   \D [1 0]})

(defn add-lim [p delta]
  (if (or (and (= delta -1) (= p 0))
          (and (= delta 1) (= p 2)))
      p
      (+ p delta)))

(defn move [pos cmd]
  (map add-lim pos (gradients cmd)))

(defn apply-cmds [cmds]
  (get-in field 
          (reduce move [1 1] cmds)))
          

(defn solve
  ([cmds-list] (map apply-cmds cmds-list))
  ([] (solve input)))

