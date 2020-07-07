(ns vicarious.word2vec-test
  (:require [clojure.test :refer :all]
            [vicarious.word2vec :refer :all]
            [vicarious.plotter :as plt]
            [vicarious.text-processor :as tp]
            [clojure.core.matrix :as m]
            [clojure.string :as str]
            [clojure.core.matrix.stats :as stats]))

; ==========================================
;; utils

(def HIGH_PRECISION 0.2)

(def LOW_PRECISION 0.5)

(def START "<START>")

(def END "<END>")

(def corpus (->> [["All", "that", "glitters", "isn't", "gold"],
                  ["All's", "well", "that", "ends", "well"]]
                 (map #(concat [START] % [END]))))

(def corpus-words (sort (concat [START]
                                ["All", "ends", "that", "gold", "All's", "glitters", "isn't", "well"]
                                [END])))

(def stopwords "test/vicarious/misc/stopwords")

(defn most-freq-words [corpus n]
  (let [stops (->> (slurp stopwords) (str/split-lines) set)]
    (->> corpus
         tp/bow
         (remove #(contains? (conj stops START END) (key %)))
         (sort-by val >)
         (take n))))
; ==========================================
;; tests

(deftest test-distinct-words
  (let [words (sort (distinct-words corpus))]
    (is (= (count corpus-words) (count words)))
    (is (= corpus-words words))))


(deftest test-co-occurrence-matrix
  (testing "should return a symmetric matrix M of shape (unique words) with entries as the context and word to row/col mapping"
    (let [word->idx' (zipmap corpus-words (range (count corpus-words)))
          expected-M (m/array [[0., 0., 0., 0., 0., 0., 1., 0., 0., 1.,],
                               [0., 0., 1., 1., 0., 0., 0., 0., 0., 0.,],
                               [0., 1., 0., 0., 0., 0., 0., 0., 1., 0.,],
                               [0., 1., 0., 0., 0., 0., 0., 0., 0., 1.,],
                               [0., 0., 0., 0., 0., 0., 0., 0., 1., 1.,],
                               [0., 0., 0., 0., 0., 0., 0., 1., 1., 0.,],
                               [1., 0., 0., 0., 0., 0., 0., 1., 0., 0.,],
                               [0., 0., 0., 0., 0., 1., 1., 0., 0., 0.,],
                               [0., 0., 1., 0., 1., 1., 0., 0., 0., 1.,],
                               [1., 0., 0., 1., 1., 0., 0., 0., 1., 0.,]])
          {:keys [M word->idx]} (co-occurrence-matrix corpus 1)]
      (is (= word->idx' word->idx))
      (is (= (m/shape expected-M) (m/shape M)))
      (is (true? (m/e== expected-M M))))))

(deftest test-dim-reduction
  (testing "should return a reduced M matrix to k dimension"
    (let [A (m/array [[1 2 3 4 5 6 7 8 9 10]
                      [11 12 13 14 15 16 17 18 19 20]
                      [21 22 23 24 25 26 27 28 29 30]])
          expected (m/array [[18.52157747 6.47697214]
                             [49.81310011 1.91182038]
                             [81.10462276 -2.65333138]])
          M (reduce-to-dim 2 A)]
      (is (true? (m/equals expected M LOW_PRECISION))))))

(deftest test-dim-reduction-corpus
  (testing "should return reduced M matrix to k dimension, on given corpus data"
    (let [{:keys [M]} (co-occurrence-matrix corpus 1)
          reduced (reduce-to-dim 2 M)]
      (is (= (m/shape reduced) [10 2])))))

(deftest test-plot-embeddings
  (testing "should plot (scatter-plot) embeddings of words in 2-dim, 4x words in rectangle like, 1x in middle"
    (let [M (m/array [[1, 1]
                      [-1, -1]
                      [1, -1]
                      [-1, 1]
                      [0, 0]])
          words ["bucharest" "its" "the" "time" "of"]
          word->idx (zipmap words (range (count words)))]
      (plt/plot-embeddings M word->idx "simple-test" words))))

(def omer-path "test/vicarious/train/omer-data/")

(deftest test-plot-embeddings-omer
  (testing "should plot a co-occurrence sample of most frequent words in omer's data"
    (let [omer-corpus (tp/corpus omer-path)
          window 2
          {:keys [M word->idx]} (co-occurrence-matrix omer-corpus window)
          M-normd (->> (reduce-to-dim 2 M) (map #(m/normalise %)) m/matrix)
          words (most-freq-words omer-corpus 10)]
      (plt/plot-embeddings M-normd word->idx (str "co-occurrence with n= " window) (keys words)))))

(deftest test-similar-words
  (testing "should return most n cosine-similar words"
    (let [words '("a" "b" "c" "d")
          word->idx (zipmap words (range (count words)))
          M [[1 2 3 4],
             [5 6 7 8]
             [9 10 11 12]
             [13 14 15 16]]
          v1 (m/get-row M 0)
          v2 (m/get-row M 1)
          expected '("b c")]
      (is (true? (m/equals [(stats/cosine-similarity v1 v2)] [(cosine-sim v1 v2)] LOW_PRECISION))
          (= expected (similar-words M word->idx "a" 2))))))

(deftest test-sigmoid
  (let [x (double (rand-int 1))]
    (is (= (m/logistic x) (sigmoid x)))))

(deftest test-softmax
  (let [v [0.6 1.1 -1.5 1.2 3.2 -1.1]
        exp-v [0.055 0.090 0.0067 0.10 0.74 0.010]
        m [[1 2] [3 4]]
        exp-m [[0.26894142, 0.73105858],
               [0.26894142, 0.73105858]]]
    (is (true? (m/equals exp-v (softmax v) HIGH_PRECISION)))
    (is (true? (m/equals exp-m (softmax m) HIGH_PRECISION)))))

; ==========================================