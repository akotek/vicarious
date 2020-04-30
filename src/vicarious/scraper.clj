(ns vicarious.scraper
  (:import (org.jsoup Jsoup)
           (org.jsoup.select Elements)
           (org.jsoup.nodes Element)))

(def BASE-URL "https://lyricstranslate.com")

(def URL (str BASE-URL "/en/omer-adam-lyrics.html"))

(defn get-page [url]
  (.get (Jsoup/connect url)))

(defn get-elems [page css]
  (.select page css))

(defn extract-links []
  (for [e (get-elems (get-page URL) "a[href]")
        :when (and (= (.attr e "class") "lang")
                   (= (.text e) "English"))]
    (str BASE-URL (.attr e "href"))))

(defn get-text [url]
  (let [elems (get-elems (get-page url) "div#songtranslation > .translate-node-text")
        title (.text (get-elems elems ".title-h2"))
        text (.text (get-elems elems ".ltf > .par"))]
  {:title title
   :text text}))

(defn scrape []
  (for [song (extract-links)]
    (get-text song)))