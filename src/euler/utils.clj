(ns euler.utils
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :refer [split trim]]))

(declare sieve prime?)

(defn divisible? [n divisor] (= 0 (mod n divisor)))

(defn prime-factor [n]
  {:post [(every? prime? %)]}
  (let [f1 (->> (range 2 (math/floor (min (inc (math/sqrt n))
                                          (inc (/ n 2)))))
             (filter #(divisible? n %))
             (first))
        f2 (if f1 (/ n f1))]
    (if f1
      (concat (prime-factor f1) (prime-factor f2))
      [n])))

(defn proper-divisors [n]
  (filter (partial divisible? n) (range 1 (inc (math/round (/ n 2))))))

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

(defn fermat? [n]
  (letfn [(probable-prime? [n a]
            (= (mod (- (math/expt a n) a) n) 0))]
    (let [a (nth (range 1 n) (rand-int (dec n)))]
      (probable-prime? n a))))

(defn prime? [n]
  (if (or (= 1 n)
          (get @prime-cache n)
          (and (not-any? (partial divisible? n) (keys @prime-cache))
               (every? identity (repeatedly 3 #(fermat? n)))
               (empty? (filter
                         #(< (apply max (keys @prime-cache)) %)
                         (factors n)))))
    (do
      (if (not= 1 n) (swap! prime-cache assoc n true))
      true)
    false))

(defn multiples
  ([n] (multiples n n))
  ([n start-at] (let [multiple (+ start-at n)]
                  (cons multiple (lazy-seq (multiples n multiple))))))

(defn sieve
  ([] (sieve 100))
  ([n] (sieve n (apply sorted-map (interleave (filter odd? (range 2 n))
                                              (repeat :maybe-prime)))))
  ([n pool]
   (loop [pool pool]
     (let [prime (ffirst (filter #(= (val %) :maybe-prime) pool))
           ms (if prime (multiples prime))]
       (if prime
         (let [multiples (take-while (partial > n) ms)]
           (println "prime:" prime)
           (recur (-> (apply (partial dissoc pool) multiples)
                    (assoc prime :prime))))
         pool)))))

;; problem 17 -- pascal's triangle
(defn pairs [row]
  (partition-all 2 1 row))

(defn pascal-row [row]
  (->> (pairs row)
    (map #(apply + %))
    (cons 0)))

(defn pascal
  ([] (pascal [0 1 0]))
  ([row] (cons row (lazy-seq (pascal (pascal-row row))))))

;; problems 18 and 67
(defn get-triangle [string]
  (let [triangle (->> (split string #"\n")
                   (map (fn [line]
                          (map #(Integer. %) (split (trim line) #" "))))
                   (map-indexed (fn [i m]
                                  (map-indexed (fn [j v] [[j i] v]) m)))
                   (apply concat)
                   (apply concat)
                   (apply hash-map))
        cached (atom {})]
    (letfn [(point [x y]
              (let [value (triangle [x y] nil)]
                (if value
                  {:value value
                   :position [x y]}
                  nil)))
            (starting-point []
              (point 0 0))
            (parents-of [{[x y] :position}]
              (cond
                (= x y 0) []
                (= x 0) [(point x (dec y))]
                :else [(point (dec x) (dec y)) (point x (dec y))]))
            (path-value [{value :value [x y] :position :as me}]
              (when me
                (let [pv (or (@cached [x y])
                             (+ value
                                (apply
                                  max
                                  (conj
                                    (filter identity
                                            (map path-value (parents-of me)))
                                    0))))]
                  (swap! cached assoc [x y] pv)
                  pv)))
            (max-path-row [row]
              (apply max (for [x (range (inc row))]
                           (do
                             (println x)
                             (path-value (point x row))))))]
      {:point point
       :starting-point starting-point
       :parents-of parents-of
       :path-value path-value
       :max-path-row max-path-row})))
