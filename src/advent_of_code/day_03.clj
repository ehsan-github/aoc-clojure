(ns advent-of-code.day-03
  (:require [clojure.string :as str]))

(defn incX [[x y]] [(inc x) y])
(defn decX [[x y]] [(dec x) y])
(defn incY [[x y]] [x (inc y)])
(defn decY [[x y]] [x (dec y)])

(def m {:R incX
        :L decX
        :U incY
        :D decY})

(defn nextRound
  [isSecond]
  (fn
    [acc [dirFn n]]
    (reduce
     (fn
       [{:keys [curr visited crossed steps]} _]
       (let [next (dirFn curr)
             steps (inc steps)]
         {:curr next
          :visited (if (or isSecond (contains? visited next))
                     visited
                     (assoc visited next steps))
          :steps steps
          :crossed (if (and isSecond (contains? visited next))
                     (assoc crossed next (if (contains? crossed next)
                                           (update crossed next #(conj % [steps (visited next)]))
                                           [[steps (get visited next)]]))
                     crossed)}))
     acc
     (range n))))

(defn computeCrossed [[p1 p2]]
  (let
   [r1 (reduce (nextRound false)
               {:curr [0 0], :visited {[0 0] 0} :crossed {} :steps 0}
               p1)]
    (reduce (nextRound true)
            (assoc r1 :curr [0 0] :steps 0)
            p2)))

(defn common-part [input]
  (->>
   input
   (#(str/split % #"\n"))
   (map #(str/split % #","))
   (map (fn [x] (map (fn [s] [((keyword (subs s 0 1)) m) (Integer/parseInt (subs s 1))]) x)))
   computeCrossed
   :crossed))

(defn part-1
  "Day 03 Part 1"
  [input]
  (->>
   input
   common-part
   keys
   (map (fn [[x y]] (+ (Math/abs x) (Math/abs y))))
   (reduce min)))

(defn part-2
  "Day 03 Part 2"
  [input]
  (->>
   input
   common-part
   vals
   (map (fn [x] (map (fn [[x y]] (+ (Math/abs x) (Math/abs y))) x)))
   flatten
   (reduce min)))

(def sample3 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

(part-2 sample3)
(part-1 sample3)