(ns advent-of-code.day-04
  (:require [clojure.string :as str]))

(defn has-repeat [x] (> 6 (count (set x))))
(defn acceptable-1? [x]
  (let [y (str/split (str x) #"")]
    (and (= y (sort y)) (has-repeat y))))

(defn acceptable-2? [x]
  (let [y (str/split (str x) #"")
        g (group-by identity y)]
    (and
     (= y (sort y))
     (< (count g) 6)
     (some #(= 2 (count %)) (vals g))
     (has-repeat y))))

(defn part-1
  "Day 04 Part 1"
  [input]
  (->>
   input
   (#(str/split % #"-"))
   (map #(Integer/parseInt %))
   ((fn [[s, e]] (reduce (fn [acc x] (if (acceptable-1? x) (+ acc 1) acc)) 0 (range s (inc e)))))))

(defn part-2
  "Day 04 Part 2"
  [input]
  (->>
   input
   (#(str/split % #"-"))
   (map #(Integer/parseInt %))
   ((fn [[s, e]] (reduce (fn [acc x] (if (acceptable-2? x) (inc acc) acc)) 0 (range s (inc e)))))))
