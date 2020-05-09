(ns covid19.plot-sim
  (:require [covid19.simulation :refer [graph->counts]]))

(def config
  "Tufte-similar look."
  {:background "#fffff8"
   :axis {:labelFont "Gill Sans" :titleFont "Gill Sans"}
   :legend {:labelFont "monospace" :titleFont "Gill Sans"}
   :header {:labelFont "Gill Sans" :titleFont "Gill Sans"}
   :mark {:labelFont "Gill Sans" :titleFont "Gill Sans"}
   :title {:labelFont "Gill Sans" :titleFont "Gill Sans"}})

(def color-scale
  {:domain ["S" "E" "I" "R" "Q"]
   :range ["#377eb8" "#ff7f00" "#e41a1c"
           "#4daf4a" "#984ea3"]})

(defn vglite-stacked-bar
  [history]
  (#?(:cljs clj->js
      :clj identity)
   {:$schema "https://vega.github.io/schema/vega-lite/v4.json"
    :config config
    :data {:values history}
    :transform
    [{:filter "datum.step % 5 == 0"}
     {:calculate "datum.step / 5" :as :day}]
    :mark :bar
    :width {:step 8}
    :encoding
    {:y
     {:aggregate :sum
      :field :count
      :type :quantitative
      :axis {:title "population"}
      :stack :normalize
      }
     :x {:field :day :type :ordinal}
     :order {:field :order :type :quantitative}
     :color
     {:field :state
      :type :nominal
      :scale color-scale}}}))

(defn vglite-2-row-line-charts
  [history n-nodes]
  (let [;; make sure the graph has the final width from the start
        history (conj history {:state :fake :step 300})
        spec {:mark :line
              :encoding
              {:y
               {:field :norm_count
                :type :quantitative
                ;; :axis {:title "population"} -- ASSOCed
                ;; :scale {:type :log} -- ASSOCed
                }
               :x {:field :day :type :quantitative}
               :order {:field :order :type :quantitative}
               :color
               {:field :state
                :type :nominal
                :scale color-scale}}}
        ]
    (#?(:cljs clj->js
        :clj identity)
     {:$schema "https://vega.github.io/schema/vega-lite/v4.json"
      :config config
      :data {:values history}
      :transform
      [{:filter "datum.count != 0"}
       {:calculate (str "datum.count/" n-nodes) :as :norm_count}
       {:calculate "datum.step / 5" :as :day}]
      :vconcat
      [(-> spec
           (assoc-in [:encoding :y :scale] {:type :linear})
           (assoc-in [:encoding :y :axis] {:grid false :title "Population"})
           (assoc-in [:encoding :x :axis] {:grid false}))
       (-> spec
           (assoc-in [:encoding :y :scale] {:type :log})
           (assoc-in [:encoding :y :axis] {:title "Population (log scale)"})
           (assoc-in [:encoding :x :axis] {:grid false}))]})))
