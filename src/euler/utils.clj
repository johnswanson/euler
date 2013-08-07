(ns euler.utils
  (:require [clojure.math.numeric-tower :as math]))

(declare sieve prime?)

(defn divisible? [n divisor] (= 0 (mod n divisor)))

(defn factor [n]
  {:post [(every? prime? %)]}
  (let [f1 (->> (range 2 (math/floor (min (inc (math/sqrt n))
                                          (inc (/ n 2)))))
             (filter #(divisible? n %))
             (first))
        f2 (if f1 (/ n f1))]
    (if f1
      (concat (factor f1) (factor f2))
      [n])))

(defn factors [n]
  (filter #(= 0 (mod n %)) (nnext (range (inc (math/round (/ n 2)))))))

(defn rev-order-factors [n]
  (map #(/ n %) (factors n)))

(defn only-odd [c]
  (filter odd? c))

(defn prime-factors [n]
  (filter prime? (only-odd (rev-order-factors n))))

(defn largest-prime-factor [n]
  (first (prime-factors n)))

(def prime-cache (atom {}))

(defn set-prime-cache! [n]
  (reset! prime-cache (apply hash-map (interleave (sieve n)
                                                  (repeat true)))))

(defn prime? [n]
  (if (or (get @prime-cache n)
          (and (empty? (filter
                        #(< (apply max (keys @prime-cache)) %)
                        (factors n)))
               (not-any? (partial divisible? n) (keys @prime-cache))))
    (do
      (if (not= 1 n) (swap! prime-cache assoc n true))
      true)
    false))

(defn multiples
  ([n] (multiples n n))
  ([n start-at] (let [multiple (+ start-at n)]
                  (cons multiple (lazy-seq (multiples n multiple))))))

(defn sieve
  [n]
  (loop [pool (apply sorted-map (interleave (nnext (range n))
                                            (repeat :maybe-prime)))]
    (let [[smallest _] (first (filter (fn [[k v]] (= v :maybe-prime)) pool))
          ms (if smallest (take-while #(< % n) (multiples smallest)))]
      (if smallest
        (do
          (recur
            (-> (if (empty? ms)
                  pool
                  (apply (partial assoc pool)
                         (interleave ms
                                     (repeat :composite))))
              (assoc smallest :prime))))
        (keys (filter (fn [[k v]] (= v :prime)) pool))))))
