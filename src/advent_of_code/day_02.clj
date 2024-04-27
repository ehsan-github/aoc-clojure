(ns advent-of-code.day-02
  (:require [clojure.string :as str]))

(defn compute
  [input]
  (let [allValidStarts (for [i (range 0 (count input) 4)] i)]
    (reduce
     (fn [input i]
       (if (= 99 (nth input i)) (reduced input)
           (let [fun (if (= 1 (nth input i)) + *)
                 a1 (nth input (nth input (+ i 1)))
                 a2 (nth input (nth input (+ i 2)))
                 pos (nth input (+ i 3))]
             (assoc input pos (fun a1 a2)))))
     input
     allValidStarts)))

(defn part-1
  "Day 02 Part 1"
  [input]
  (->> input
       (#(str/split % #","))
       (map #(Integer/parseInt %))
       vec
       (#(assoc % 1 12 2 2))
       compute))


(defn part-2
  "Day 02 Part 2"
  [input]
  (->> input
       (#(for [x (range 0 99)
               y (range 0 99)
               :when (= 19690720 (part-1 (assoc % 1 x 2 y)))]
           [x y]))))