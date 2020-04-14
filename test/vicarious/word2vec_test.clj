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

; ==========================================