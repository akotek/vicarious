(ns vicarious.classifier-test
  (:require [clojure.test :refer :all]
            [vicarious.bayes-classifier :refer :all]))

;; utils
; ==========================================

(def simple-path "/Users/kotek/IdeaProjects/vicarious/test/train/simple-data/")

(def polarity-path "/Users/kotek/IdeaProjects/vicarious/test/train/polarity-data/")

(defn pick-sample [m k ks]
  (update m k (fn [xs]
                (map #(select-keys % ks) xs))))

(defn round-decimal [xs]
  (map #(format "%.3f" %) xs))

(defn max-idx [xs]
  (->> xs
       (map-indexed vector)
       (apply max-key second)
       first))

(defn argmax [classes results]
  (-> results
      (max-idx)
      (get classes)))

; ==========================================
;; tests

(defn classes [path]
  (map #(str path %) ["neg"
                      "pos"]))

(deftest test-train-small
  (testing "tests train on small data-set, should return priors, likelihoods and vocab (ignored in test due size)"
    ;; to pass > remove Math/log from classifier-class
    (let [expected {:classes '("neg" "pos")
                    :priors '(3/5 2/5)
                    :likelihoods '({"predictable" 1/17
                                   "no"          1/17
                                   "fun"         1/34},
                                   {"predictable" 1/29
                                    "no"          1/29
                                    "fun"         2/29})}]
      (is (= expected (-> (train (classes simple-path))
                          (dissoc :V)
                          (pick-sample :likelihoods ["predictable" "no" "fun"])))))))

(deftest test-prediction-small
  (testing "tests prediction on small data-set, should return sentiments with neg > pos"
    ;; to pass > remove Math/log from classifier-class
    (let [{:keys [priors likelihoods V]} (train (classes simple-path))
          test-doc (str simple-path "test/a")
          expected [(float (* 3/5 2/34 2/34 1/34))
                    (float (* 2/5 1/29 1/29 2/29))]]
      (is (= (round-decimal expected) (round-decimal (predict test-doc priors likelihoods V)))))))

(deftest test-prediction-big
  (testing "tests prediction on Pang & Lee polarity data-set, should classify correctly pos/neg for simple + narcos docs"
    (let [{:keys [priors likelihoods V classes]} (train (classes polarity-path))
          test1 (str polarity-path "test/a1")
          test2 (str polarity-path "test/a2")
          test3-imdb (str polarity-path "test/narcos-mex-pos")
          test4-imdb (str polarity-path "test/narcos-mex-neg")]
       (= "pos" (->> (predict test1 priors likelihoods V) (argmax classes)))
       (= "neg" (->> (predict test2 priors likelihoods V) (argmax classes)))
       (= "pos" (->> (predict test3-imdb priors likelihoods V) (argmax classes)))
       (= "neg" (->> (predict test4-imdb priors likelihoods V) (argmax classes))))))

; ==========================================