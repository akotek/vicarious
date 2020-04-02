(ns vicarious.tokenizer-test
  (:require [clojure.test :refer :all]
            [vicarious.tokenizer :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as s]))

;; utils
; ==========================================

(def num-tests 100)

(defn un-frequencies [m]
  (mapcat
    (fn [[k v]]
      (repeat v k)) m))

; ==========================================
;; tests

(def s "The 2 QUICK Brown-foxes jumped over the lazy_ dog's bone.")

(def tokens ["the", "2", "quick", "brownfoxes", "jumped", "over", "lazy_", "dogs", "bone"])

(deftest test-tokenize
  (testing "given a string, should return vector of tokens (defined as words) lower-cased, w/o whitespaces and punctuation,
  '_' represents end-of-word"
    (is (= (tokenize s) tokens))))

(defspec test-tokenize-props num-tests
         (prop/for-all [text gen/string-ascii]
                       (let [res (first (tokenize text))]
                         (and (<= (count res) (count text))
                              (some? (re-find #"^[a-zA-Z0-9_]*$" res))))))

(deftest test-bow
  (testing "given words, should return a map of lowered-case words with their frequency in the document"
    (is (= (word-count tokens) (frequencies tokens)))))

(defspec test-bow-prop num-tests
         (prop/for-all [v (gen/such-that (complement empty?) (gen/vector
                                                               (gen/such-that (complement s/blank?) gen/string-alphanumeric)))]
                       (let [lowered (map #(s/lower-case %) v)
                             res (word-count lowered)]
                         (and (<= (count (keys res)) (count lowered))
                              (= (count lowered) (reduce + (vals res)))
                              (= (set lowered) (set (keys res)))
                              (= (set lowered) (set (un-frequencies res)))))))

; ==========================================