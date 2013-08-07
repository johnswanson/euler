(ns euler.core
  (:require [clojure.math.numeric-tower :as math])
  (:require [euler.utils :as utils]))

(utils/set-prime-cache! 1000)

(defn smallest-multiple [n]
  (->> (range 2 (inc n))
    (map (comp frequencies utils/factor))
    (reduce #(merge-with max %1 %2))
    (map (fn [[k v]] (math/expt k v)))
    (reduce *)))

(defn combine [current add]
  (merge-with max current add))
