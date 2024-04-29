(ns advent-of-code.day-06
  (:require [clojure.string :as str]))


(defn add-node [acc [a b]]
  (assoc acc b (fn [obj]
                 (let [v (get obj a)]
                   (if (fn? v)
                     (inc (v obj))
                     (inc v))))))

(defn get-vals [obj] (map (fn [v] (if (fn? v) (v obj) v)) (vals obj)))

(defn part-1
  "Day 06 Part 1"
  [input]
  (->> input
       (#(str/split % #"\n"))
       (map #(str/split % #"\)"))
       (reduce add-node {"COM" 0})
       get-vals
       (reduce +)))


;; part-2
(defn mmap [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn get-vals-2 [start l p obj] (let [curr (get obj start)
                                       children (:children curr)]
                                   (if (nil? children)
                                     (assoc obj start {:level l :p p})
                                     (assoc
                                      (reduce (fn [obj child] (get-vals-2 child (inc l) start obj)) obj children)
                                      start
                                      {:level l :children children :p p}))))

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
       (#(str/split % #"\n"))
       (map #(str/split % #"\)"))
       (group-by first)
       (#(mmap % (fn [x] {:children (map second x) :level nil})))
       (get-vals-2 "COM" 0 nil)
       (find-steps "YOU" "SAN" 0)))

