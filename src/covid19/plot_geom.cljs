;; This file is unused, kept here for replacing vega-lite in the future
(ns covid19.plot-geom
  (:require [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.color.core :as col]
            [thi.ng.math.core :as m :refer [PI]]))

(defn test-equation
  [const x] [x (* (Math/cos (* const x)) (Math/sin (* x x x)))])

(def viz-spec
  {:x-axis (viz/linear-axis
            {:domain [(- PI) PI]
             :range  [50 580]
             :major  (/ PI 2)
             :minor  (/ PI 4)
             :pos    250})
   :y-axis (viz/linear-axis
            {:domain      [-1 1]
             :range       [250 20]
             :major       0.2
             :minor       0.1
             :pos         50
             :label-dist  15
             :label-style {:text-anchor "end"}})
   :grid   {:attribs {:stroke "#caa"}
            :minor-y true}
   :data   [
            ;; reference line - 0.5, blue
            {:values (map (partial test-equation 0.5) (for [t (m/norm-range 200)] (* 2 PI (- t 0.5))))
             :attribs {:fill "none" :stroke "#0af" :stroke-width "2px"}
             :layout  viz/svg-line-plot}
            ;; changing line - red
            {:values (map (partial test-equation 0.5) (for [t (m/norm-range 200)] (* 2 PI (- t 0.5))))
             :attribs {:fill "none" :stroke "red"}
             :layout  viz/svg-line-plot}]})

(def svg (svg/svg {:width 600 :height 320}
                  (viz/svg-plot2d-cartesian viz-spec)))

(defn gen-svg
  [const]
  (let [data (for [t (m/norm-range 200)]
               (test-equation const (* 2 PI (- t 0.5))))
        spec (update-in viz-spec [:data 1 :values]
                        (fn [_ new] new) data)]
    (svg/svg {:width 600 :height 320}
             (viz/svg-plot2d-cartesian spec))))

(defn sim-svg
  []
  (let [spec {:x-axis (viz/linear-axis
                       {:domain [0 30]
                        :range  [50 580]
                        :major  5
                        :minor  1
                        :pos    250})
              :y-axis (viz/linear-axis
                       {:domain      [0 1]
                        :range       [250 20]
                        :major       0.2
                        :minor       0.1
                        :pos         50
                        :label-dist  15
                        :label-style {:text-anchor "end"}})
              :grid   {:attribs {:stroke "#caa"}
                       :minor-y true}
              :data   [{:values  (map (fn [x] [x (rand)]) (range 0 30 0.2))
                        :attribs {:fill "none" :stroke "#0af"}
                        :layout  viz/svg-line-plot}
                       {:values (map (juxt #(/ % 5)
                                           #(- 1 (* % (/ 0.6 150))))
                                     (range 51))
                        :attribs {:fill "none" :stroke "red"}
                        :layout  viz/svg-line-plot}
                       ]}

        ;; {:x-axis (viz/linear-axis
             ;;           {:domain [0 30]
             ;;            :range  [50 590]
             ;;            :major 7
             ;;            :minor 1
             ;;            :pos    550})
             ;;  :y-axis (viz/log-axis
             ;;           {:domain      [0 1]
             ;;            :range       [550 20]
             ;;            :pos         50
             ;;            :major       0.1
             ;;            :label-dist  15
             ;;            :label-style {:text-anchor "end"}
             ;;            })
             ;;  :grid   {:attribs {:stroke "#caa"}
             ;;           :minor-x true
             ;;           :minor-y true}
             ;;  :data   [;; S - [[0 1] ... [30 0.4]]
                       ;; {:values (map (juxt #(/ % 5)
                       ;;                     #(- 1 (* % (/ 0.6 150))))
                       ;;               (range 151))
                       ;;  :attribs {:fill "#0af" :stroke "none"}
                       ;;  :layout  viz/svg-scatter-plot}
             ;;           {:values  (map (juxt #(/ % 7) (fn [_] 0)) (range 0 200 2))
             ;;            :attribs {:fill "none" :stroke "#f60"}
             ;;            :shape   (viz/svg-triangle-down 6)
             ;;            :layout  viz/svg-scatter-plot}
             ;;           ]}
        ]
    (->> spec
         (viz/svg-plot2d-cartesian)
         (svg/svg {:width 600 :height 320}))))
