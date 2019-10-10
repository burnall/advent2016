(ns adv.t11)

(def input {
  :elev 0,
  :plan [[{:dev :gen, :el :th} {:dev :chip, :el :th}, {:dev :gen, :el :pl}, {:dev :gen, :el :sr}]
         [{:dev :chip, :el :pl}, {:dev :chip, :el :sr}]
         [{:dev :gen, :el :pt}, {:dev :chip, :el :pt}, {:dev :gen, :el :rt}, {:dev :chip, :el :rt}]
         []]})

(defn all-obj-top? [plan]
  (->> plan
       (count)
       (dec)
       (subvec plan 0)
       (every? empty?)))

(defn good? [objs]
  (->> objs
       (reduce (fn [[gens chips ] {:keys [dev el]}]
                 (if (= dev :gen) 
                   [(conj gens el) chips]
                   [gens (conj chips el)]))
               [#{} #{}])
       ((fn [[gens chips]] 
         (or (zero? (count gens))
             (empty? (clojure.set/difference chips gens)))))))

(defn get-pairs [n]
  (->> n
       (range)
       (map (fn [i] [i n]))))

(defn get-selections [n]
  (->> n
       (range)
       (mapcat get-pairs)))
