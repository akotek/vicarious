(ns vicarious.word2vec
  (:require [clojure.set :as set]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as lin]))

(m/set-current-implementation :vectorz)

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
    (vec (sort words))))

(defn co-occurrence-matrix [corpus n]
  (let [words (distinct-words corpus)
        num-words (count words)
        word->idx (zipmap words (range num-words))
        shape (vec (repeat 2 num-words))
        M (reduce (fn [M' coll]
                    (co-occur M' coll word->idx n))
                  (m/zero-array shape) corpus)]
    {:M         M
     :word->idx word->idx}))

(defn reduce-to-k-dim [M k]
  (let [{:keys [U S]} (lin/svd M {:return [:U :S]})
        U' (->> U (m/columns) (take k) (m/transpose))
        S' (->> S (take k) (m/diagonal-matrix))]
    (m/mmul U' S')))

(defn sigmoid [x]
  (/ 1
     (+ 1 (Math/exp (- x)))))

(defn softmax [x]
  (let [f (fn [v] (let [em (m/exp v)]
                    (m/div em (m/esum em))))]
    (if (> (count (m/shape x)) 1)
      (mapv f x)
      (f x))))

; ============================================================