(ns day03.solution
  (:require [tools :refer [file->str]]))

(defn parse [it] (re-seq #"\d+" it))

(defn next-digit [it len acc]
  (let [digits (map (comp parse-long str) it)
        windows (partition len 1 digits)]
    (loop [idx 0
           remaining-windows windows
           [max-idx max-digit] [0 (ffirst windows)]]
      (if (empty? remaining-windows)
        [(subs it (inc max-idx))
         (dec len)
         (conj acc max-digit)]
        (let [[[this] & other-windows] remaining-windows]
          (recur (inc idx)
                 other-windows
                 (if (> this max-digit)
                   [idx this]
                   [max-idx max-digit])))))))

(defn jolt [it len]
  (loop [s it, p len, acc []]
    (if (= len (count acc)) acc
        (let [[s' p' acc'] (next-digit s p acc)]
          (recur s' p' acc')))))

(defn solve [it len]
  (->> it
       (map #(-> % (jolt len) (->> (apply str) parse-long)))
       (apply +)))

(defn -main [day]
  (let [input (->> day file->str parse)]
    {:part1 (solve input 2) :part2 (solve input 12)}))


(comment

  (let [test-input (parse "987654321111111
811111111111119
234234234234278
818181911112111")]
    (assert (= 357 (solve test-input 2)))
    (assert (= 3121910778619 (solve test-input 12))))

  )
