(ns day04.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [split-lines]]))

(def ADJACENT [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn grid [s]
  (->> s
       split-lines
       (mapv #(keep-indexed (fn [i ch] (when (= ch \@) i)) %))
       (mapv set)))

(defn count-adjacent [grid y x]
  (reduce (fn [cnt [dy dx]]
            (cond-> cnt
              (contains? (get grid (+ y dy)) (+ x dx)) inc))
          0
          ADJACENT))

(defn fewer-than-four? [grid y x]
  (and (contains? (get grid y) x)
       (< (count-adjacent grid y x) 4)))

(defn part1 [grid]
  (let [len (count grid)]
    (reduce + (for [y (range len)]
                (count (filter #(fewer-than-four? grid y %) (get grid y)))))))

(defn clear [grid for-removal]
  (reduce
   (fn [acc [y x]] (update acc y disj x))
   grid for-removal))

(defn adjacent [y x]
  (mapv (fn [[dy dx]] [(+ y dy) (+ x dx)]) ADJACENT))

(defn part2 [grid]
  (let [len (count grid)
        initial-cells (set (for [y (range len) x (get grid y)] [y x]))]
    (loop [current grid, to-check initial-cells, total-removed 0]
      (if (empty? to-check)
        total-removed
        (let [for-removal (filter (fn [[y x]] (fewer-than-four? current y x)) to-check)
              next-grid (clear current for-removal)
              neighbors (set (mapcat (fn [[y x]] (adjacent y x)) for-removal))]
          (recur next-grid neighbors (+ total-removed (count for-removal))))))))

(defn -main [day]
  (let [input (->> day file->str grid)]
    {:part1 (part1 input) :part2 (part2 input)}))

(comment

  (let [test-input (grid "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")]
    (assert (= 13 (part1 test-input)))
    (assert (= 43 (part2 test-input))))

  )
