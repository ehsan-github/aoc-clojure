(ns advent-of-code.day-06
  (:require [clojure.string :as str]))

(defn mmap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn create-tree [start l p obj]
  (let [curr (get obj start)
        children (:children curr)]
    (if (nil? children)
      (assoc obj start {:level l :p p})
      (assoc
       (reduce (fn [obj child] (create-tree child (inc l) start obj)) obj children)
       start
       {:level l :children children :p p}))))

(defn common-part
  "Day 06 Part 2"
  [input]
  (->> input
       (#(str/split % #"\n"))
       (map #(str/split % #"\)"))
       (group-by first)
       (mmap (fn [x] {:children (map second x) :level nil}))
       (create-tree "COM" 0 nil)))

(defn part-1
  "Day 06 Part 1"
  [input]
  (->> input
       common-part
       vals
       (map :level)
       (reduce +)))

(defn find-steps [s e steps obj]
  (let [start (get obj s)
        end (get obj e)] (if (= (:p start) (:p end))
                           steps
                           (if (>= (:level start) (:level end))
                             (find-steps (:p start) e (inc steps) obj)
                             (find-steps s (:p end) (inc steps) obj)))))

(defn part-2
  "Day 06 Part 2"
  [input]
  (->> input
       common-part
       (find-steps "YOU" "SAN" 0)))

