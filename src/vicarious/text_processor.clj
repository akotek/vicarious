(ns vicarious.text-processor
  (:require [clojure.java.io :as io]
            [vicarious.tokenizer :as tk]
            [vicarious.scraper :as scp]))

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

(defn create-corpus-mem []
  (for [song (scp/scrape-omer)]
    (->> song :text tk/tokenize meta-data)))

(defn create-corpus-disk [path]
  (for [song (scp/scrape-omer)]
    (spit (str path (.replace (:title song) " " "-")) (:text song))))