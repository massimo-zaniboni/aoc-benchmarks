; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.09b
  (:require
    [clojure.string :as str]))

(defn xor [a b] (and (not (and a b)) (or a b)))
(defn or-else [x default] (if (some? x) x default))
(def input (->> *in*
                (slurp)
                (str/split-lines)
                (mapv (partial mapv #(Integer/parseInt (str %))))))

(defn at [v [x y]] (some-> v (get y) (get x)))
(defn is-minimum [v [x y]]
  (let [m (at v [x y])]
    (every? #(< m %) (for [dx [-1 0 +1]
                           dy [-1 0 +1]
                           ; Only shift on X or Y, not both
                           :when (or (not= dx 0) (not= dy 0))]
                       (or-else (at v [(+ x dx) (+ y dy)]) 10)))))
(defn all-coords [v]
  (for [x (range (count (first v)))
        y (range (count v))]
    [x y]))

(defn search-basin [v xy]
  (loop [latest   #{xy}
         explored #{}]
    (if (empty? latest)
      explored
      (recur
        ; Increase L0-norm by 1 and check that the resulting
        ; coordinates do not have height 9
        (set (for [coord latest
                   dx [-1 0 +1]
                   dy [-1 0 +1]
                   :when (xor (= dx 0) (= dy 0))
                   :let [new-coord  (mapv + coord [dx dy])
                         new-height (at v new-coord)]
                   :when (some? new-height)
                   :when (not= new-height 9)
                   :when (not (explored coord))]
               new-coord))
        (into explored latest)))))

(println (->> input
              (all-coords)
              (filter (partial is-minimum input))
              (map (partial search-basin input))
              (map count)
              (sort >)
              (take 3)
              (apply *)))
