(ns vicarious.word2vec
  (:require [clojure.set :as set]
            [clojure.core.matrix :as m]))

;TODO
;1. post co-occurrence in SO clojure
;2. transform test-corpus to meir-ariel-corpus
; ============================================================
;; utils

(defn count->array [indices shape]
  (let [values (-> (m/row-count indices) (m/new-vector) (m/add 1))]
    (-> (m/zero-array shape)
      (m/set-indices indices values))))

(defn co-occur [coll word->idx shape n]
  (->> coll
       (partition (inc n) 1)
       (reduce (fn [M1 xs]
                 (let [indices (reduce (fn [coll w]
                                         (let [fst (word->idx (first xs))
                                               nxt (word->idx w)]
                                           (conj coll
                                                 [fst nxt] [nxt fst])))
                                       [] (next xs))
                       M2 (count->array indices shape)]
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