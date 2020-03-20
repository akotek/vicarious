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
(def max-tries 50)

(defn un-frequencies [m]
  (mapcat
    (fn [[k v]]
      (repeat v k)) m))

(defn lowercase? [s]
  (and
    (not (s/blank? s))
    (= s (s/lower-case s))))

; ==========================================
;; tests

(def s "The 2 QUICK Brown-foxes jumped over the lazy dog's bone.")

(def tokens ["the", "2", "quick", "brownfoxes", "jumped", "over", "lazy", "dogs", "bone"])

(deftest test-tokenize
  (testing "given a string, should return vector of tokens, defined as words (lowercase, w/o whitespaces, punctuation and dup's)"
    (is (= (tokenize s) tokens))))

(defspec tokenize-is-alphanumeric num-tests
         (prop/for-all [text gen/string-ascii]
                       (let [res (first (tokenize text))]
                         (and (<= (count res) (count text))
                              (some? (re-find #"^[a-zA-Z0-9]*$" res))))))

(deftest test-bow
  (testing "given words, should return a map of lowered-case words with their frequency in the document"
    (is (= (frequencies tokens) (bow tokens)))))

(defspec bow-properties num-tests
         (prop/for-all [v (gen/such-that (complement empty?) (gen/vector gen/string-alphanumeric))]
                       (let [lowered (filter #((complement s/blank?) %) (map #(s/lower-case %) v))
                             res (bow lowered)]
                         (and (<= (count (keys res)) (count lowered))
                              (= (count lowered) (reduce + (vals res)))
                              (= (set lowered) (set (keys res)))
                              (= (set lowered) (set (un-frequencies res)))))))

; ==========================================