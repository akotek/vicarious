(ns vicarious.word2vec
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as lm]))

(m/set-current-implementation :vectorz)

; ============================================================
;; utils

(defn occurrence-indices [corpus word->idx n]
  (mapcat (fn [line]
            (mapcat (fn [[w & words]]
                      (map #(vector (word->idx w) (word->idx %)) words))
                    (partition (inc n) 1 line))) corpus))

(defn inc' [M [x y]]
  (m/mset M x y
          (inc (m/mget M x y))))
; ============================================================
;; API

(defn distinct-words [nested-v]
  (->> nested-v
       (mapcat #(identity %))
       set))

(defn co-occurrence-matrix [corpus n]
  (let [word->idx (zipmap (sort (distinct-words corpus)) (range))
        shape (vec (repeat 2 (count word->idx)))
        M (->> (occurrence-indices corpus word->idx n)
               (reduce (fn [M' loc]
                         (-> (inc' M' loc)
                             (inc' (reverse loc))))
                       (m/zero-array shape)))]
    {:M         M
     :word->idx word->idx}))

(defn reduce-to-dim [k M]
  (let [{:keys [U S]} (lm/svd M {:return [:U :S]})
        U' (->> U (m/columns) (take k) (m/transpose))
        S' (->> S (take k) (m/diagonal-matrix))]
    (m/mmul U' S')))

(defn cosine-sim [v1 v2]
  (m/div
    (m/dot v1 v2)
    (m/dot (lm/norm v1) (lm/norm v2))))

(defn similarity [M word->idx w1 w2]
  (cosine-sim (m/get-row M (word->idx w1))
              (m/get-row M (word->idx w2))))

(defn similar-words [M word->idx w n]
  (let [words (keys (dissoc word->idx w))
        sim (for [w' words]
              (similarity M word->idx w w'))]
    (->> (zipmap sim words)
         (sort-by >)
         (take n)
         vals)))

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