(ns advent-of-code.day-01
  (:require [clojure.string :as string]))

(def sample "12\n14\n1969\n100756")

(defn compute-fuel
  [mass]
  (-> mass
      (/ 3)
      (int)
      (- 2)))


(defn part-1
  "Day 01 Part 1"
  [input]
  (->> input
       string/split-lines
       (map #(Integer/parseInt %))
       (map compute-fuel)
       (reduce +)))

(part-1 sample)

(defn compute-fuel2
  [mass]
  (-> mass
      compute-fuel
      (#(cond
          (< % 9) %
          :else (+ % (compute-fuel2 %))))))

(defn part-2
  "Day 01 Part 2"
  [input]
  (->> input
       string/split-lines
       (map #(Integer/parseInt %))
       (map compute-fuel2)
       (reduce +)))

(part-2 sample)
