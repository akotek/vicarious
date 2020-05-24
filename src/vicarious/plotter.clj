(ns vicarious.plotter
  (:require [clojure.core.matrix :as m]))

(use '(incanter core charts))

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