; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.06b
  (:require
    [clojure.string :as str]))

(defn parse [line]
  (vec (map #(Integer/parseInt %) (str/split line #","))))

(defn prdbg [x] (do (println x) x))
(defn ifnil [x y] (if (nil? x) y x))

(defn step [n freqs]
  (loop [k 0
         freqs freqs]
    (if (= k n) freqs
      (let [; decrease all keys by one
            timers (reduce-kv #(assoc %1 (dec %2) %3) {} freqs)
            new (get timers -1)]
        (recur (inc k) (if (some? new)
                         (-> timers
                             (update 8 #(+ (ifnil % 0) new))
                             (update 6 #(+ (ifnil % 0) new))
                             (dissoc -1))
                         timers))))))

(let [freqs (->> (read-line) (parse) (frequencies))
      n (->> (read-line) (Integer/parseInt))]

     (println (-> (step n freqs)
                  (vals)
                  (apply +))))
