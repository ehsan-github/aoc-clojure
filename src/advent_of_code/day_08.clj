(ns advent-of-code.day-08
  (:require [clojure.string :as str]))

(defn count-x [x arr] (count (filter #(= % x) arr)))

(defn part-1
  "Day 08 Part 1"
  [input]
  (->> input
       (#(partition-all (* 6 25) %))
       (reduce (fn [a b] (let [z-a (count-x \0 a)
                               z-b (count-x \0 b)]
                           (if (< z-a z-b) a b))))
       (#(* (count-x \1 %) (count-x \2 %)))))

(defn find-first
  [f coll]
  (first (filter f coll)))

(defn part-2
  "Day 08 Part 2"
  [input]
  (->> input
       (#(partition-all (* 6 25) %))
       (apply map vector)
       (map #(find-first (fn [x] (not= x \2)) %))
       (#(partition-all 25 %))
       (map #(map (fn [x] (if (= x \1) "*" " ")) %))
       (map #(apply str %))
       (str/join "\n")))

"****  ***     **   **   ***  
    *  *  *     *  *  *  *  * 
   *   ***      *  *  *  ***  
  *    *  *     *  ****  *  * 
 *     *  *  *  *  *  *  *  * 
 ****  ***    **   *  *  *** "
"ZBJAB"