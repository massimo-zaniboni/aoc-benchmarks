; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.07b
  (:require
    [clojure.string :as str]))

(defn compute-cost [pos x]
  (->> pos
       (map #(Math/abs (- x %)))
       (map #(/ (* % (+ % 1)) 2)) ; Gauss's formula: k*(k+1)/2
       (apply +)))

(println (as-> (read-line) pos
           (str/split pos #",")
           (mapv #(Integer/parseInt %) pos)
           ; (apply min-key (partial compute-cost pos) (range (apply max pos)))))
           (map (partial compute-cost pos) (range (apply max pos)))
           (apply min pos)))
