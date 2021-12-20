; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.03b)

(defn parse-line [line] (map #(Integer/parseInt (str %)) line))

(defn most-common-bit-at [idx values]
  (->> values
       (reduce #(update %1 (nth %2 idx) inc) {0 0, 1 0})
       (apply max-key val)
       (key)))

(def orig-values
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (map parse-line)))

(def num-bits (count (first orig-values)))

(defn calculate-rating [rating-at]
  (loop [values orig-values
         idx 0]
    (if (or (= idx num-bits) (= (count values) 1))
      (first values)
      (let [value-ok (fn [value] (= (nth value idx) (rating-at idx values)))]
        (recur (filter value-ok values) (inc idx))))))

(defn from-bin [xs]
  (reduce
    #(case %2
       0 (* 2 %1)
       1 (inc (* 2 %1))) xs))

(def oxygen-rating (calculate-rating #(most-common-bit-at %1 %2)))
(def co2-rating (calculate-rating #(- 1 (most-common-bit-at %1 %2))))
(println (* (from-bin oxygen-rating) (from-bin co2-rating)))
