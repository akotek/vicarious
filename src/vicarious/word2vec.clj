(ns vicarious.word2vec
  (:require [clojure.set :as set]
            [clojure.core.matrix :as m]))

;TODO
;1. post co-occurrence in SO clojure
;2. transform test-corpus to meir-ariel-corpus
; ============================================================
;; utils

(defn count->array [i j shape]
  (-> (m/zero-array shape)
      (m/set-indices [[i j]
                      [j i]] [1. 1.])))

(defn co-occur [coll word->idx shape n]
  (->> coll
       (partition (inc n) 1)
       (reduce (fn [M1 xs]
                 (let [e1 (word->idx (first xs))
                       e2 (word->idx (nth xs n))
                       M2 (count->array e1 e2 shape)]
                   (m/add M1 M2)))
               (m/zero-array shape))))

; ============================================================
;; API

(defn distinct-words [nested-v]
  (let [words (reduce (fn [s xs]
                        (set/union s (set xs)))
                      #{} nested-v)]
    {:words     (vec  (sort words))
     :num-words (count words)}))

(defn co-occurrence-matrix [corpus n]
  (let [{:keys [words num-words]} (distinct-words corpus)
        word->idx (zipmap words (range num-words))
        shape (vec (repeat 2 num-words))
        M (reduce (fn [M' coll]
                    (m/add M'
                           (co-occur coll word->idx shape n)))
                  (m/zero-array shape) corpus)]
    {:M        M
     :word2idx word->idx}))

; ============================================================