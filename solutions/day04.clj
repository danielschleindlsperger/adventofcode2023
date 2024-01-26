(ns adventofcode2023.day04
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math :as math]
            [clojure.pprint :refer [pprint]]))

(def example-input
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
")

(defn card-worth
  [card]
  (let [winning-count (count (:matching-numbers card))]
    (loop [i 0
           worth 0]
      (if (= i winning-count)
        worth
        (recur (inc i) (if (zero? worth) 1 (* 2 worth)))))))

(defn parse-card-line
  [s]
  (let [[_ card-num] (re-find #"Card\s*(\d*)\:" s)
        [_ winning-numbers] (re-find #"\:\s*(\d.*) \|" s)
        winning-numbers (->> (str/split winning-numbers #" ")
                             (map str/trim)
                             (filter (complement str/blank?))
                             (map parse-long)
                             (set))
        [_ given-numbers] (re-find #"\|\s*(\d.*)" s)
        given-numbers (->> (str/split given-numbers #" ")
                           (map str/trim)
                           (filter (complement str/blank?))
                           (map parse-long)
                           (set))
        card {:card-num (parse-long card-num),
              :winning-numbers winning-numbers,
              :given-numbers given-numbers,
              :matching-numbers (set/intersection winning-numbers
                                                  given-numbers)}]
    (-> card
        (assoc :card-worth (card-worth card)))))

(comment
  (parse-card-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))

(defn parse-cards
  [input]
  (->> (str/split-lines input)
       (filter (complement str/blank?))
       (map parse-card-line)))

(comment
  (parse-cards example-input))

(defn total-points
  [input]
  (->> (parse-cards input)
       (map #(get % :card-worth))
       (reduce +)))

(comment
  (total-points example-input) ;; => 13
  (total-points (slurp "../inputs/day04.txt")) ;; => 32609
)

;; Part 2
;;

(defn add-cards
  [all-cards curr-card]
  (let [matching-nums (count (:matching-numbers curr-card))
        card-idx (dec (:card-num curr-card))]
    (subvec all-cards (inc card-idx) (inc (+ card-idx matching-nums)))))

(defn total-card-count
  [input]
  (let [cards (vec (parse-cards input))
        cards-by-num (reduce (fn [by-num card]
                               (assoc by-num (:card-num card) card))
                       {}
                       cards)]
    (loop [owned-cards cards
           i 0]
      (if (>= i (count owned-cards))
        (count owned-cards)
        (let [curr-card (get owned-cards i)]
          (recur (vec (concat owned-cards (add-cards cards curr-card)))
                 (inc i)))))))

(comment
  (total-card-count example-input) ;; => 30
  (total-card-count (slurp "../inputs/day04.txt")) ;; =>
)
