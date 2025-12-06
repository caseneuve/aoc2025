(ns day01.solution
  (:require [tools :refer [file->str]]))

(def start 50)

(defn zeros [pos steps dir]
  (let [p (if (= "L" dir) (mod (- 100 pos) 100) pos)]
    (quot (+ p steps) 100)))

(defn solution [input]
  (loop [p1 0, p2 0, pos start, ii input]
    (if (empty? ii) [p1 p2]
        (let [[_ d n] (first ii)
              op (if (= "L" d) - +)
              n (parse-long n)
              npos (mod (op pos n) 100)
              extra (zeros pos n d)
              acc1 (cond-> p1 (zero? npos) inc)
              acc2 (+ p2 extra)]
          (recur acc1 acc2 npos (rest ii))))))

(defn -main [day]
  (let [input (->> day file->str (re-seq #"(.)(\d+)"))
        [p1 p2] (solution input)]
    {:part1 p1 :part2 p2}))


(comment
  (let [test-input "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"
        [part1 part2] (->> test-input (re-seq #"(.)(\d+)") solution)]
    (assert (= part1 3))
    (assert (= part2 6)))
  )
