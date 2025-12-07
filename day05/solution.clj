(ns day05.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [split]]))

(defn consolidate [ranges]
    (reduce
      (fn [acc [s e]]
        (let [[ps pe] (peek acc)]
          (if (and ps (<= s pe))
            (conj (pop acc) [ps (max e pe)])
            (conj acc [s e]))))
      []
      ranges))

(defn parse [it]
  (let [[ranges ids] (split it #"\n\n")
        numbers #(->> % (re-seq #"\d+") (map parse-long))]
    [(->> ranges numbers (partition 2) (sort-by first) consolidate)
     (numbers ids)]))

;; Could use binary search, but `some` is fast enough with given inputs
(defn in-range? [ranges n]
  (some (fn [[start end]] (<= start n end)) ranges))

(defn part1 [ranges ids]
  (count (filter #(in-range? ranges %) ids)))

(defn part2 [ranges]
  (apply + (map (fn [[a b]] (inc (- b a))) ranges)))

(defn -main [day]
  (let [[ranges ids] (->> day file->str parse)]
    {:part1 (part1 ranges ids) :part2 (part2 ranges)}))


(comment

  (let [[ranges ids] (parse "3-5
10-14
16-20
12-18

1
5
8
11
17
32")]
    (assert (=  3 (part1 ranges ids)))
    (assert (= 14 (part2 ranges))))

  )
