(ns vicarious.bayes-classifier
  (:require [clojure.java.io :as io]
            [vicarious.tokenizer :as tokenizer]
            [clojure.set :as set]
            [clojure.string :as str]))

; ============================================================
;; utils

(defn vocab [bows]
  (->> bows
       (reduce (fn [s1 s2]
                 (set/union s1 (set (keys s2))))
               #{})))

(defn priors [classes]
  (let [num-files (map (fn [p]
                         (-> p (io/file) (.listFiles) (count)))
                       classes)]
    (map #(Math/log
            (/ % (apply + num-files)))
         num-files)))

(defn likelihood [bow w words-count voc-count]
  {w (Math/log
       (/ (inc (get bow w 0))
          (+ words-count voc-count)))})

(defn likelihoods [bows V]
  (map #(reduce
          (fn [m w] (merge m
                           (likelihood % w (reduce + (vals %)) (count V))))
          {} V) bows))

; ============================================================
;; API

(defn train [classes]
  (let [priors (priors classes)
        bows (map tokenizer/bow-dir classes)
        V (vocab bows)
        likelihoods (likelihoods bows V)]
    {:V           V
     :classes     (map #(last (str/split % #"/")) classes)
     :priors      priors
     :likelihoods likelihoods}))

(defn predict [test-doc priors likelihoods V]
  (let [words (with-open [rdr (io/reader test-doc)]
                (reduce (fn [words line]
                          (concat words
                                  (->> line
                                       (tokenizer/tokenize)
                                       (filter #(contains? V %)))))
                        '() (line-seq rdr)))]
    (map (fn [pr lh]
           (reduce (fn [s w]
                     (+ (float s) (float (get lh w))))
                   pr words))
         priors likelihoods)))
; ============================================================