(ns day07.solution
  (:require [tools :refer [file->str]]))

(defn grid [input]
  (butlast
   (reduce
    (fn [[g S [y x]] ch]
      (if (= ch \newline) [g S [(inc y) 0]]
          [(assoc g [y x] ch)
           (if (= ch \S) [y x] S)
           [y (inc x)]]))
    [{} nil [0 0]] input)))

(defn next-moves [grid [y x]]
  (let [[ny nx :as nxt] [(inc y) x]]
    (if (= \^ (grid nxt))
      {:split nxt :moves [[ny (dec nx)] [ny (inc nx)]]}
      {:split nil :moves [nxt]})))

(defn count-splits-bfs [grid start]
  (loop [Q (conj clojure.lang.PersistentQueue/EMPTY start), splits #{}, seen #{}]
    (if (empty? Q)
      (count splits)
      (let [pos (peek Q)
            {:keys [split moves]} (next-moves grid pos)
            moves (->> moves (filter #(contains? grid %)) (remove #(contains? seen %)))]
        (recur (into (pop Q) moves), (if split (conj splits split) splits), (into seen moves))))))

(defn count-paths-dfs-memo [grid start]
  (let [cache (atom {})]
    (letfn [(dfs [pos]
              (if-let [cached (@cache pos)]
                cached
                (let [{:keys [moves]} (next-moves grid pos)
                      valid-moves (filter #(contains? grid %) moves)
                      result (if (empty? valid-moves)
                               1
                               (reduce + (map dfs valid-moves)))]
                  (swap! cache assoc pos result)
                  result)))]
      (dfs start))))

(defn -main [day]
  (let [[G S] (->> day file->str grid)]
    {:part1 (count-splits-bfs G S) :part2 (count-paths-dfs-memo G S)}))


(comment

  (let [[G S] (grid ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")]
    (assert (= 21 (count-splits-bfs G S)))
    (assert (= 40 (count-paths-dfs-memo G S))))

  )
