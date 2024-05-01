(ns advent-of-code.day-10
  (:require [clojure.string :as str]))

(defn create-arr
  [str-ls]
  (reduce
   (fn [acc x]
     (let [xx (nth str-ls x)]
       (conj acc
             (reduce
              (fn [acc2 y] (conj acc2 (if (= \# (nth xx y)) 0 -1)))
              [] (range (count xx))))))
   [] (range (count str-ls))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))
(gcd 0 8)

(defn between-two
  [[x1 y1] [x2 y2]]
  (let [x-diff (Math/abs (- x1 x2))
        y-diff (- y2 y1)
        gcd-num (gcd x-diff y-diff)
        step-x (/ x-diff gcd-num)
        step-y (/ y-diff gcd-num)]
    (for [s (range 1 gcd-num)]
      (if (< x1 x2)
        [(+ x1 (* s step-x)) (+ y1 (* s step-y))]
        [(- x1 (* s step-x)) (+ y1 (* s step-y))]))))

(defn see-each-other?
  [[x1 y1] [x2 y2] data]
  (let [target (nth (nth data y2) x2)]
    (if (= target -1)
      false
      (let [in-between (between-two [x1 y1] [x2 y2])]
        (every? (fn [[x y]] (= -1 (nth (nth data y) x))) in-between)))))

(defn compute-sight
  [acc [x y]] (let [char (nth (nth acc y) x)
                    not-visited (for [yy (range (count acc))
                                      xx (range (count (first acc)))
                                      :when (or (and (= yy y) (> xx x))
                                                (> yy y))]
                                  [xx yy])]
                (if (= char -1)
                  acc
                  (reduce
                   (fn [acc2 [xx yy]]
                     (if (see-each-other? [x y] [xx yy] acc2)
                       (update-in
                        (update-in acc2 [yy xx] inc)
                        [y x] inc)
                       acc2))
                   acc
                   not-visited))))


(defn part-1
  "Day 10 Part 1"
  [input]
  (->> input
       (str/split-lines)
       create-arr
       ((fn [arr]
          (let [all (for [y (range (count arr))
                          x (range (count (first arr)))]
                      [x y])]
            (reduce compute-sight arr all))))
       flatten
       (apply max)))

(defn part-2
  "Day 10 Part 2"
  [input]
  input)