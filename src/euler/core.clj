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

(defn leap-year? [y] (and (utils/divisible? y 4)
                          (or (not (utils/divisible? y 100))
                              (utils/divisible? y 400))))

(defn days-in [m y]
  (case m
    0 31
    1 (if (leap-year? y) 29 28)
    2 31
    3 30
    4 31
    5 30
    6 31
    7 31
    8 30
    9 31
    10 30
    11 31))

(defn next-month [m y d]
  (let [month (mod (inc m) 12)
        year (if (= month 0) (inc y) y)
        day (mod (+ d (days-in m y)) 7)]
    {:month month
     :year year
     :day day}))

(defn days
  ([] (days {:month 0 :year 1900 :day 1}))
  ([{:keys [month year day] :as m}]
   (cons m (lazy-seq (days (next-month month year day))))))

(defn problem-19
  "You are given the following information, but you may prefer to do some
  research for yourself.

  1 Jan 1900 was a Monday.
  Thirty days has September,
  April, June and November.
  All the rest have thirty-one,
  Saving February alone,
  Which has twenty-eight, rain or shine.
  And on leap years, twenty-nine.
  A leap year occurs on any year evenly divisible by 4, but not on a century
  unless it is divisible by 400.
  How many Sundays fell on the first of the month during the twentieth century
  (1 Jan 1901 to 31 Dec 2000)?"
  []
  (letfn [(before-2001 [{year :year}] (< year 2001))
          (before-1901 [{year :year}] (< year 1901))
          (sunday? [{day :day}] (= day 0))]
    (->> (days)
      (drop-while before-1901)
      (take-while before-2001)
      (filter sunday?)
      (count))))

(defn problem-28
  "Starting with the number 1 and moving to the right in a clockwise direction
  a 5 by 5 spiral is formed as follows:

  21 22 23 24 25
  20  7  8  9 10
  19  6  1  2 11
  18  5  4  3 12
  17 16 15 14 13

  It can be verified that the sum of the numbers on the diagonals is 101.

  What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
  formed in the same way?"
  ([] (problem-28 1001))
  ([sides]
   (letfn [(diagonal [square]
             (if (<= square 1)
               '(1)
               (let [corners (iterate #(- % (dec (math/sqrt square))) square)]
                 (concat (take 4 corners)
                         (diagonal (nth corners 4))))))]
     (reduce + (diagonal (* sides sides))))))


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
