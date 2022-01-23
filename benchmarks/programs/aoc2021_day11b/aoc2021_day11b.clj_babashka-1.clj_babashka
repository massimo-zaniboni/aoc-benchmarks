; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.11b)

(def n 10)
(def input (->> (slurp *in*)
                (seq)
                (filter (partial not= \newline))
                (mapv #(Integer/parseInt (str %)))))
(defn at [[x y]] (+ (* y n) x))
(defn coord [c] [(rem c n) (quot c n)])
(defn valid [[x y]] (and (< x n) (>= x 0) (< y n) (>= y 0)))
(defn energy-increase [x]
  (if (= x :flashed) :flashed (inc x)))
(defn has-to-flash [x]
  (if (= x :flashed) false (> x 9)))
(defn single-step [xs]
  (loop [energy  (mapv inc xs)]
    (let
      [flashing-elements   (keep-indexed #(when (has-to-flash %2) %1) energy)
       surrounding         (vec (for [e     flashing-elements
                                      dx    [-1 0 +1]
                                      dy    [-1 0 +1]
                                      :when (or (not= dx 0) (not= dy 0))
                                      :let  [[x y] (coord e)
                                             shifted [(+ x dx) (+ y dy)]]
                                      :when (valid shifted)]
                                  (at shifted)))
       updated-flashing    (reduce #(assoc! %1 %2 :flashed)
                                   (transient energy) flashing-elements)
       updated-surrounding (persistent!
                             (reduce #(assoc! %1 %2 (energy-increase (get %1 %2)))
                                     updated-flashing surrounding))]
      (if
        (empty? flashing-elements)
        (mapv #(if (= % :flashed) 0 %) energy)
        (recur updated-surrounding)))))

(println (->> input
              (iterate single-step)
              (take-while #(not (every? (partial = 0) %)))
              (count)))
