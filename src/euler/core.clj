(ns euler.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :refer [split trim]]
            [euler.utils :as utils]
            [clojure.math.combinatorics :as combo :refer [permutations]]))


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
