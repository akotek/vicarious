(ns vicarious.word2vec-test
  (:require [clojure.test :refer :all]
            [vicarious.word2vec :refer :all]
            [clojure.core.matrix :as m]))

; ==========================================
;; utils

(def START "<START>")

(def END "<END>")

(def corpus (->> [["All", "that", "glitters", "isn't", "gold"],
                  ["All's", "well", "that", "ends", "well"]]
                 (map #(concat [START] % [END]))))

(def corpus-words (sort (concat [START]
                                ["All", "ends", "that", "gold", "All's", "glitters", "isn't", "well"]
                                [END])))

; ==========================================
;; tests

(deftest test-distinct-words
  (testing "should return sorted distinct words of corpus and it num of words"
    (let [{:keys [words num-words]} (distinct-words corpus)]
      (is (= (count corpus-words) num-words))
      (is (= corpus-words words)))))


(deftest test-co-occurrence-matrix
  (testing "should return a symmetric matrix M of shape (unique words) with entries as the context and word to row/col mapping"
    (let [word->idx (zipmap corpus-words (range (count corpus-words)))
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
          {:keys [M word2idx]} (co-occurrence-matrix corpus 1)]
      (is (= word->idx word2idx))
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
          M (reduce-to-k-dim A 2)
          diff-impl (truncated-svd A 2)]
      (is (true? (m/equals expected M 0.5)))
      (is (true? (m/equals expected diff-impl 0.5))))))

(deftest test-dim-reduction-corpus
  (testing "should return reduced M matrix to k dimension, on given corpus data"
    (let [{:keys [M]} (co-occurrence-matrix corpus 1)
          reduced (reduce-to-k-dim M 2)]
      (is (= (m/shape reduced) [10 2])))))

; ==========================================