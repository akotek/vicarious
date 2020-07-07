(ns vicarious.word2vec
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as lm]
            [clojure.core.matrix.protocols :as mp]))

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
  (->> nested-v
       (mapcat #(identity %))
       set))

(defn co-occurrence-matrix [corpus n]
  (let [words (sort (distinct-words corpus))
        num-words (count words)
        word->idx (zipmap words (range num-words))
        shape (vec (repeat 2 num-words))
        M (reduce (fn [M' coll]
                    (co-occur M' coll word->idx n))
                  (m/zero-array shape) corpus)]
    {:M         M
     :word->idx word->idx}))

(defn reduce-to-k-dim [M k]
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

(defn naive-softmax [center-vecs out-idx out-vecs data]
  ;https://stats.stackexchange.com/questions/253244/gradients-for-skipgram-word2vec
  (let [y-hat (softmax (m/dot out-vecs center-vecs)) ;P(o|c) = exp(u_oT * v_c) / (sigma-w (exp (u_wT * v_c)) Nx1
        y'-y (m/sub ; Nx1
               (m/get-row y-hat out-idx) 1)]
    {:loss                 (-> y-hat (m/get-row out-idx) mp/log m/sub) ;-log(P(o|c)
     :gradient-center-vec  (m/dot (m/transpose out-vecs) ;U.T(y'-y) Dx1
                                  y'-y)
     :gradient-outside-vec "gradient-outside-vec"}))        ;Nx1 dot 1xD > NxD
; ============================================================