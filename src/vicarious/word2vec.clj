(ns vicarious.word2vec
  (:require [clojure.set :as set]
            [clojure.core.matrix :as m]))

;TODO
;1. post co-occurrence in SO clojure
;2. transform test-corpus to meir-ariel-corpus
; ============================================================
;; utils

(defn matrix-values [indices]
  (-> (m/row-count indices)
      (m/new-vector)
      (m/add 1)))

(defn co-occur [M coll word->idx n]
  (->> coll
       (partition (inc n) 1)
       (reduce (fn [M' xs]
                 (let [indices (reduce (fn [coll w]
                                         (let [fst (word->idx (first xs))
                                               nxt (word->idx w)]
                                           (conj coll [fst nxt] [nxt fst])))
                                       [] (next xs))]
                   (m/set-indices M' indices (matrix-values indices))))
               M)))

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
                    (co-occur M' coll word->idx n))
                  (m/zero-array shape) corpus)]
    {:M        M
     :word2idx word->idx}))

; ============================================================