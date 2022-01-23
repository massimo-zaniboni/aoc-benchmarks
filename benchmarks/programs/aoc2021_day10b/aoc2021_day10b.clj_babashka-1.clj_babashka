; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.10a
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]))

(defn score [x]
  (case x
    \) 1
    \] 2
    \} 3
    \> 4
    nil 0))

(defn matching [x]
  (case x
    \( \)
    \[ \]
    \{ \}
    \< \>
    \) \(
    \] \[
    \} \{
    \> \<))

(defn opening [x]
  (case x
    \( true
    \[ true
    \{ true
    \< true
    \) false
    \] false
    \} false
    \> false))

(defn opened-stack [line]
  (reduce (fn [acc chr]
            (if (opening chr)
              (conj acc chr)
              (if
                (= (peek acc) (matching chr)) (pop acc)
                (reduced nil)))) () line))

(defn score-to-close [pending-opened]
  (reduce (fn [total-score chr]
            (+ (* 5 total-score) (score (matching chr)))) 0 pending-opened))

(defn median [xs] (nth (sort xs) (/ (count xs) 2)))

(println (->> *in*
              (java.io.BufferedReader.)
              (line-seq)
              (map opened-stack)
              (filter some?)
              (mapv score-to-close)
              (median)))
