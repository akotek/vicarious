(ns vicarious.word2vec
  (:require [clojure.set :as set]
            [clojure.core.matrix :as m]))

(use '(clojure.core.matrix.operators))

;TODO
;1. post co-occurrence in SO clojure
;2. transform test-corpus to meir-ariel-corpus
; ============================================================
;; utils

(defn deep-merge-with [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))

(defn co-occur [xs n]
  (->> xs
       (partition (inc n) 1)
       (map (fn [xs]
              (let [nth' (nth xs n)
                    fst (first xs)]
                {fst  {nth' 1}
                 nth' {fst 1}})))
       (apply merge-with merge)))

(defn count->array [m n]
  (m/array
    (for [i (range n)]
      (if (contains? (set (keys m)) i)
        (get m i)
        0))))
; ============================================================
;; API

(defn distinct-words [nested-v]
  (let [words (reduce (fn [s xs]
                        (set/union s (set xs)))
                      #{} nested-v)]
    {:words     (vec (sort words))
     :num-words (count words)}))

(defn context [corpus n]
  (reduce (fn [bigm m]
            (deep-merge-with + bigm (co-occur m n)))
          {} corpus))

(defn co-occurrence-matrix
  ([corpus]
   (co-occurrence-matrix corpus 2))
  ([corpus n]
   (let [{:keys [words num-words]} (distinct-words corpus)
         word->idx (zipmap words (range num-words))
         shape (vec (repeat 2 num-words))
         M (->> (context corpus n)
                (into (sorted-map))
                (#(do (println %) %))
                vals
                (map #(count->array (set/rename-keys % word->idx) (first shape)))
                (m/array))]
     {:M (m/add
           (apply m/zero-matrix shape) M)
      :word2idx word->idx})))
; ============================================================