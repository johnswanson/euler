(ns euler.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :refer [split trim lower-case] :as string]
            [euler.utils :as utils]
            [clojure.java.io :as io]
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

(defn problem-22
  "
  Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
  containing over five-thousand first names, begin by sorting it into
  alphabetical order. Then working out the alphabetical value for each name,
  multiply this value by its alphabetical position in the list to obtain a name
  score.

  For example, when the list is sorted into alphabetical order, COLIN, which is
  worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
  would obtain a score of 938 × 53 = 49714.

  What is the total of all the name scores in the file?"
  ([] (problem-22 "problem-22.txt"))
  ([filename]
   (letfn [(value-char [c] (- (int c) (dec (int \a))))
           (value [s] (reduce + (map value-char (lower-case s))))
           (score [i s] (* (inc i) (value s)))]
     (->> (split (slurp (str (io/resource filename))) #",")
       (map #(string/replace % #"\"" ""))
       (sort)
       (map-indexed score)
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
