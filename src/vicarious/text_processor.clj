(ns vicarious.text-processor
  (:require [clojure.java.io :as io]
            [vicarious.tokenizer :as tk]))

(defn meta-data [tokens]
  (concat ["<START>"] tokens ["<END>"]))

(defn tokens [file]
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (mapcat tk/tokenize)
         doall
         meta-data)))

(defn corpus [dir]
  (->> dir
       io/file
       file-seq
       rest
       (map tokens)))

(defn bow [corpus]
  (->> corpus
       (reduce (fn [m doc]
                 (merge-with + m (frequencies doc)))
               {})))