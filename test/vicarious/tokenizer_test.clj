(ns vicarious.tokenizer-test
  (:require [clojure.test :refer :all]
            [vicarious.tokenizer :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

;; utils
; ==========================================

; ==========================================
;; tests

(deftest test-tokenize
  (testing "grammar based tokenize, removes whitespaces, punctuation, duplications and lower cases tokens"
    (let [s "The 2 QUICK Brown Foxes jumped over the lazy dog's bone."
          expected ["the", "2", "quick", "brown", "foxes", "jumped", "over", "lazy", "dogs", "bone"]]
      (is (= (tokenize s) expected)))))

(defspec alphanumeric-prop 100
  (prop/for-all [text gen/string-ascii]
                (let [res (first (tokenize text))]
                  (and (<= (count res) (count text))
                       (some? (re-find #"^[a-zA-Z0-9]*$" res))))))
; ==========================================