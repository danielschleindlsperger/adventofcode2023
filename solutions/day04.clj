(ns adventofcode2023.day04
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math :as math]))

(def example-input
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
")

(defn parse-card-line
  [s]
  (let [[_ winning-numbers] (re-find #"\:\s*(\d.*) \|" s)
        winning-numbers (->> (str/split winning-numbers #" ")
                             (map parse-long))
        [_ given-numbers] (re-find #"\|\s*(\d.*)" s)
        given-numbers (->> (str/split given-numbers #" ")
                           (map parse-long)
                           (filter some?))]
    {:winning-numbers (set winning-numbers),
     :given-numbers (set given-numbers)}))

(comment
  (parse-card-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))

(defn parse-cards
  [input]
  (->> (str/split-lines input)
       (filter (complement str/blank?))
       (map parse-card-line)))

(comment
  (parse-cards example-input))

(defn card-worth
  [card]
  (let [winning-count (count (set/intersection (:winning-numbers card)
                                               (:given-numbers card)))]
    (loop [i 0
           worth 0]
      (if (= i winning-count)
        worth
        (recur (inc i) (if (zero? worth) 1 (* 2 worth)))))))

(defn total-points
  [input]
  (->> (parse-cards input)
       (map card-worth)
       (reduce +)))

(comment
  (total-points example-input)
  (total-points (slurp "../inputs/day04.txt")) ;; => 32609
)
