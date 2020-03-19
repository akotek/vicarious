(ns vicarious.tokenizer
  (:require [clojure.string :as s]))

; ==========================================
;; utils

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

; ==========================================