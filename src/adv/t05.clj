(ns adv.t05)

(import java.security.MessageDigest java.math.BigInteger)

(defn md5
  [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn solve
  ([door-id] 
    (->> (range)
         (map (partial str door-id))
         (map md5)
         (filter (fn [s] (clojure.string/starts-with? s "00000")))
         (take 8)
         (map (fn [s] (nth s 5)))
         (apply str)))
  ([] (solve "ugkcyxxp")))

