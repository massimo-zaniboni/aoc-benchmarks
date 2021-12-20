; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.15b
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.data.priority-map :refer [priority-map-keyfn]]))

(def input *in*)
(def grid (->> input
               (io/reader)
               (line-seq)
               (mapcat (partial map #(Integer/parseInt (str %))))
               (vec)))
(def block-size (Math/round (Math/sqrt (count grid))))
(def n (* block-size 5))
(defn at [[x y]] (+ (* y block-size) x))
(defn get-cost [grid [x y]]
  (let [block-x  (quot x block-size)
        block-y  (quot y block-size)
        offset-x (rem x block-size)
        offset-y (rem y block-size)
        cost     (+ block-x block-y (grid (at [offset-x offset-y])))]
    (if (> cost 9) (- cost 9) cost)))
(defn all-coords [maxx maxy]
  (for [x (range maxx)
        y (range maxy)]
       [x y]))

(defn xor [a b] (and (not (and a b)) (or a b)))
(defn neighbours [[x y]]
  (for [dx    [-1 0 1]
        dy    [-1 0 1]
        :when (xor (not= dx 0) (not= dy 0))
        :let  [newx (+ x dx)
               newy (+ y dy)]
        :when (and (>= newx 0) (< newx n))
        :when (and (>= newy 0) (< newy n))]
    [newx newy]))
(defn dijkstra [cost-of start end]
  (loop [pq (assoc
              (reduce
                #(assoc %1 %2 [##Inf nil]) ; [dist, prev]
                (priority-map-keyfn first)
                (all-coords n n))
              start [0 nil])
         prevs {}]
    (let [[node [this-dist prev]] (first pq)
          popd                    (dissoc pq node)
          saved-prev              (assoc prevs node prev)]
      (if (= node end) saved-prev
        (recur
          (into popd (for [neigh                (neighbours node)
                           :let [cost           (cost-of neigh)
                                 new-neigh-dist (+ this-dist cost)
                                 old-neigh-dist (first (pq neigh))]
                           :when                (some? old-neigh-dist) ; If neighbour is still in the priority queue
                           :when                (< new-neigh-dist old-neigh-dist)]
                       [neigh [new-neigh-dist node]]))
          saved-prev 0)))))
(defn acc-path [dst edges]
  (loop [path [dst]]
    (let [prev (edges (peek path))]
             (if (some? prev)
               (recur (conj path prev))
               path))))

(def corner0 [0 0])
(def corner1 [(dec n) (dec n)])
(println (->> (dijkstra (partial get-cost grid) corner0 corner1)
              (acc-path corner1)
              (drop-last)
              (map (partial get-cost grid))
              (apply +)))
