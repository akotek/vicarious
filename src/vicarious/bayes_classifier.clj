(ns vicarious.bayes-classifier
  (:require [clojure.java.io :as io]
            [vicarious.tokenizer :as tokenizer]
            [clojure.set :as set]))

; ============================================================
;; utils

(defn priors [p1 & rest]
  (let [num-files (map (fn [p]
                      (-> p (io/file) (.listFiles) (count)))
                    (cons p1 rest))]
    (map #(/ %1 (reduce + num-files)) num-files)))

(defn likelihood [bow w words-count voc-count]
  {w (/ ((fnil inc 0) (get bow w))
        (+ words-count voc-count))})

; ============================================================
;; API

(defn train [c1 & rest]
  (let [priors (apply priors c1 rest)
        bows (map tokenizer/bow-dir (cons c1 rest))
        V (reduce #(set/union %1 (set (keys %2))) #{} bows)
        lh (map #(reduce
                   (fn [m w] (merge m
                                    (likelihood % w (reduce + (vals %)) (count V))))
                   {} V) bows)]
    {:priors priors
     :likelihoods lh}))

(defn predict [doc c1 c2 vocab]
  (let [filtered 5]))
