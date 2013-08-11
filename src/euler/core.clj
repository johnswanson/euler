(ns euler.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :refer [split trim]]
            [euler.utils :as utils]
            [clojure.set :refer [union]]
            [clojure.math.combinatorics :as combo :refer [permutations]]))

(defn problem-21
  "Amicable numbers
  Problem 21
  Let d(n) be defined as the sum of proper divisors of n (numbers less than n
  which divide evenly into n).
  If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
  each of a and b are called amicable numbers.

  For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
  55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
  71 and 142; so d(284) = 220.

  Evaluate the sum of all the amicable numbers under 10000."
  ([] (problem-21 10001))
  ([n]
   (let [cache (atom {})
         amicable? (fn [sumfactors n]
                     (let [a n
                           b (sumfactors n)]
                       (and (= a (sumfactors b))
                            (not= a b))))
         data (->> (range 2 n)
                (map (fn [n] {n (reduce + (utils/proper-divisors n))}))
                (apply merge))]
     (->> (filter (partial amicable? data) (range 2 n))
       (reduce +)))))

(defn problem-9 []
  (for [a (range 1 1000)
        b (range 1 1000)
        c (range 1 1000)
        :when (< a b c)
        :when (= 1000 (+ a b c))
        :when (= (* c c) (+ (* b b) (* a a)))]
    {:a a
     :b b
     :c c}))

(defn problem-12 []
  (first (for [n (range 1 10000000)
               :let [t (reduce + (range (inc n)))
                     factors (frequencies (utils/prime-factor t))
                     exps (reduce * (map inc (vals factors)))]
               :when (< 500 exps)]
           {:n n
            :t t
            :factors (utils/prime-factor t)})))

(defn square [n] (math/expt n 2))

(defn sum-squares [& numbers]
  (reduce #(+ %1 (math/expt %2 2)) numbers))

(defn square-sums [& numbers]
  (-> (reduce + numbers)
    (math/expt 2)))

(defn smallest-multiple [n]
  (->> (range 2 (inc n))
    (map (comp frequencies utils/prime-factor))
    (reduce #(merge-with max %1 %2))
    (map (fn [[k v]] (math/expt k v)))
    (reduce *)))

(defn combine [current add]
  (merge-with max current add))
