(ns vicarious.plotter
  (:require [clojure.core.matrix :as m]))

(use '(incanter core charts))

(defn xs->vega-map [xs]
  (map #(hash-map :freq (val %) :word (key %)) xs))

(defn stacked-bar-vega [data]
  {:title "distribution of words in omer's corpus"
   :data {:values data}
   :mark "bar"
   :encoding {:x {:field "word"
                  :type "ordinal"
                  :sort "x"}
              :y {:field "freq"
                  :type "quantitative"}}})

(defn word-cloud-vega [data]
  {:data {:values data
          :name "data"}
   :marks [{:type "text"
            :from {:data "data"}
            :encode {:enter {:text {:field "word"
                                    :baseline {:value "alphabetic"}
                                    :align {:value "center"}}}}
            :transform [{:type "wordcloud"
                         :size [800, 400]
                         :text {:field "word"}
                         :font "Helvetica Neue, Arial"
                         :fontSize {:field "datum.freq"}
                         :fontSizeRange [10, 120]
                         :padding 2}]}]})

(defn plot-embeddings [M word->idx title words]
  (let [indices (vals (select-keys word->idx words))
        sliced (m/emap #(m/select M % :all) indices)
        x-cors (m/get-column sliced 0)
        y-cors (m/get-column sliced 1)
        plot (scatter-plot x-cors y-cors
                           :title title
                           :x-label "X"
                           :y-label "Y")]
    (view plot)
    (doseq [[x y w] (map list x-cors y-cors words)]
      (add-text plot x (+ 0.01 y) w))))

