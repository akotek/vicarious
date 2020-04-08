(ns vicarious.classifier-test
  (:require [clojure.test :refer :all]
            [vicarious.bayes-classifier :refer :all]))

;; utils
; ==========================================

(def path "/Users/kotek/IdeaProjects/vicarious/train/simple-data/")

(defn pick-sample [m k ks]
  (update m k (fn [xs]
                (map #(select-keys % ks) xs))))

(defn round-decimal [xs]
  (map #(format "%.3f" %) xs))

; ==========================================
;; tests

(def classes (map #(str path %)
                  ["neg"
                   "pos"]))

(deftest test-train-small
  (testing "tests train on small data-set, should return priors, likelihoods and vocab (ignored in test due size)"
    (let [expected {:classes '("neg" "pos")
                    :priors '(3/5 2/5)
                    :likelihoods '({"predictable" 1/17
                                   "no"          1/17
                                   "fun"         1/34},
                                   {"predictable" 1/29
                                    "no"          1/29
                                    "fun"         2/29})}]
      (is (= expected (-> (train classes)
                          (dissoc :V)
                          (pick-sample :likelihoods ["predictable" "no" "fun"])))))))

(deftest test-prediction-small
  (testing "tests prediction on small data-set, should return negative sentiment"
    (let [{:keys [priors likelihoods V]} (train classes)
          test-doc (str path "test/a")
          expected [(float (* 3/5 2/34 2/34 1/34))
                    (float (* 2/5 1/29 1/29 2/29))]]
      (is (= (round-decimal expected) (round-decimal (predict test-doc priors likelihoods V)))))))

; ==========================================