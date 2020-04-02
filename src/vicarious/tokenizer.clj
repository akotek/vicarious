(ns vicarious.tokenizer
  (:require [clojure.string :as s]
            [clojure.set :as set']
            [clojure.java.io :as io]))

; ============================================================
;; utils

(defn unigrams [xs]
  (reduce
    (fn [s w] (set'/union s (set (char-array w)))) #{} xs))

(defn pair-freqs [m]
  (reduce #(merge-with + %1 %2)
          (for [[k v] m]
            (zipmap (partition 2 1 k) (repeat v)))))

(defn count' [s x]
  (if (get s x)
    (update s x inc)
    (assoc s x 1)))

(defn word-count [words]
  (->> words
       (map #(s/lower-case %))
       (reduce count' {})))

(defn char-count [s]
  (->> s
       (char-array)
       (reduce count' {})))

(defn tokenize [text]
  (as-> text t
        (s/trim t)
        (filter #(or (Character/isSpace %) (Character/isLetterOrDigit ^Character %) (= % \_)) t)
        (apply str t)
        (s/lower-case t)
        (s/split t #"\s+")
        ;(distinct t)
        (into [] t)))

; ============================================================
;; API

(defn bow-file [file]
  (with-open [rdr (io/reader file)]
    (reduce (fn [m l]
              (as-> l line
                    (tokenize line)
                    (word-count line)
                    (merge-with + m line)))
            {} (line-seq rdr))))


; ==========================================