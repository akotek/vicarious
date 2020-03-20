(ns vicarious.tokenizer
  (:require [clojure.string :as s]
            [clojure.set :as set']))

; ==========================================
;; utils

(defn chars-set [xs]
  (reduce
    (fn [s w] (set'/union s (set (char-array w)))) #{} xs))

; ==========================================
;; API

(defn tokenize [text]
  (as-> text t
        (s/trim t)
        (filter #(or (Character/isSpace %) (Character/isLetterOrDigit ^Character %)) t)
        (apply str t)
        (s/lower-case t)
        (s/split t #"\s+")
        (distinct t)
        (into [] t)))

(defn bow [words]
  (->> words
       (map #(s/lower-case %))
       (reduce (fn [s w] (if (contains? s w)
                           (update s w inc)
                           (assoc s w 1))) {})))

; ==========================================