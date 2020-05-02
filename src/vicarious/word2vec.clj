(ns vicarious.word2vec
  (:require [clojure.set :as set]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as lin]))

(use '(incanter core charts))
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
    {:words     (vec  (sort words))
     :num-words (count words)}))

(defn co-occurrence-matrix [corpus n]
  (let [{:keys [words num-words]} (distinct-words corpus)
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

(defn plot-embeddings [M word->idx title words]
  (let [indices (vals (select-keys word->idx words))
        sliced (m/emap #(m/select M % :all) indices)
        x-cors (m/get-column sliced 0)
        y-cors (m/get-column sliced 1)
        plot (scatter-plot x-cors y-cors
                           :title title
                           :x-label "X"
                           :y-label "Y")]
    (view plot)
    (doseq [[x y w] (map list x-cors y-cors words)]
      (add-text plot x (+ 0.01 y) w))))

; ============================================================