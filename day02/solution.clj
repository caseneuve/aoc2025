(ns day02.solution
  (:require [tools :refer [file->str]]
            [clojure.math :refer [pow]]))

(defn parse [it]
  (->> it
       (re-seq #"(\d+)-(\d+)")
       (map rest)))

(defn get-ranges [[start end :as pair]]
  (let [[count-start count-end] (map count pair )]
    (if (= count-start count-end) [pair]
        [[start (->> (pow 10 count-start) dec int str)]
         [(->> (pow 10 count-start) int str) end]])))

(defn- divisors [n]
  (for [d (range 1 (inc n)) :when (zero? (mod n d))]
    d))

(defn digits->int [digits]
  (->> digits (apply str) parse-long))

(defn get-valid-ids [part [a b]]
  (let [len (count a)
        divisors (butlast (divisors len))
        [A B] (map parse-long [a b])]
    (distinct
     (flatten
      (for [divisor divisors
            :when (case part
                    :1 (= divisor (/ len 2))
                    :2 :always)]
        (let [lo (digits->int (take divisor a))
              hi (pow 10 divisor)
              multiplier (/ len divisor)]
          (for [x (range lo hi)
                :let [n (digits->int (repeat multiplier x))]
                :when (<= A n B)]
            n)))))))

(defn solve [it part]
  (->> it
       (map get-ranges)
       (apply concat)
       (map (partial get-valid-ids part))
       flatten
       (apply +)))

(defn -main [day]
  (let [input (->> day file->str parse)]
    {:part1 (solve input :1) :part2 (solve input :2)}))


(comment

  (let [test-input (parse "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")]
    (assert (= (solve test-input :1) 1227775554))
    (assert (= (solve test-input :2) 4174379265)))

  )
