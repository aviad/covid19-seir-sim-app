(ns covid19.core
  (:require [reagent.dom :as dom]
            [re-frame.core :as rf]
            [clojure.string :as str]
            ["jstat" :as jstat]
            ["vega-embed" :as vega]
            [covid19.plot-geom :as plot-geom]
            [covid19.plot-sim :as plot-sim]
            [covid19.simulation :refer [initialize simulation-step graph->counts]]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; -- Domino 1 - Event Dispatch -----------------------------------------------

;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db              ;; sets up initial application state
  :initialize                 ;; usage:  (dispatch [:initialize])
  (fn [_ _]                   ;; the two parameters are not important here, so use _
    {:n-nodes 5000            ;; What it returns becomes the new application state
     :gamma 0.2
     :mean-degree 20
     :sim-state nil
     :prob-tested-daily 0.02
     :test-symptomatic true}))

(rf/reg-event-db                ;; usage:  (dispatch [:n-nodes-change 0.123])
 :n-nodes-change
 (fn [db [_ new-value]]
   (assoc db :n-nodes new-value)))

(rf/reg-event-db
 :gamma-change
 (fn [db [_ new-value]]
   (assoc db :gamma new-value)))

(rf/reg-event-db
 :mean-degree-change
 (fn [db [_ new-value]]
   (assoc db :mean-degree new-value)))

(rf/reg-event-db
 :prob-tested-daily-change
 (fn [db [_ new-value]]
   (assoc db :prob-tested-daily new-value)))

(rf/reg-event-db
 :test-symptomatic-change
 (fn [db [_ new-value]]
   (assoc db :test-symptomatic new-value)))

(rf/reg-event-db
 :start-simulation
 (fn [db [_ _]]
   (let [{:keys [n-nodes gamma mean-degree prob-tested-daily test-symptomatic]} db
         initial-graph (initialize {:n-nodes n-nodes
                                    :gamma gamma
                                    :mean-degree mean-degree
                                    :prob-tested-daily prob-tested-daily
                                    :test-symptomatic test-symptomatic})]
    (assoc db :sim-state {:g initial-graph :history []}))))

(rf/reg-event-db
 :sim-state-change
 (fn [db [_ new-state]]
   (assoc db :sim-state new-state)))

;; -- Domino 4 - Query  -------------------------------------------------------
(rf/reg-sub
 :n-nodes
 (fn [db _]
   (:n-nodes db)))

(rf/reg-sub
 :gamma
 (fn [db _]
   (:gamma db)))

(rf/reg-sub
 :mean-degree
 (fn [db _]
   (:mean-degree db)))

(rf/reg-sub
 :prob-tested-daily
 (fn [db _]
   (:prob-tested-daily db)))

(rf/reg-sub
 :test-symptomatic
 (fn [db _]
   (:test-symptomatic db)))

(rf/reg-sub
 :sim-state
 (fn [db _]
   (:sim-state db)))

(rf/reg-sub
 :start-simulation
 (fn [db _]
   (:start-simulation db)))

;; -- Domino 5 - View Functions ----------------------------------------------

(defnp hidden-run-simulation
  []
  (when-let [sim-state @(rf/subscribe [:sim-state])]
    (when-let [history (:history sim-state)]
      (when (>= 300 (count history))
        (let [new-state (simulation-step (:g sim-state))
             step-results (flatten (map assoc
                                        (graph->counts new-state)
                                        (repeat :step)
                                        (repeat (count history))))
              updated-history (conj history step-results)]
          (rf/dispatch [:sim-state-change
                       {:g new-state :history updated-history}]))))))

(defn general-intro
  []
  [:section#general-intro
   [:h2 "Introduction"]
   [:p [:span.newthought "The article has an agent-based simulation"]
    " of COVID-19 spreading on a human-like connections network. This page allows you "
    "to experiment with this simulation, adjusting some of its parameters. "
    "It is creating a network just like the one in the "
    [:a {:href "https://www.medrxiv.org/content/10.1101/2020.04.30.20081828v1"
         :target "_blank"
         :rel "noreferrer noopener"}
     "article"]
    ", and simulates the disease spreading on that network."]
   [:p "For details, read the article"
    [:label.margin-toggle.sidenote-number {:for :article-bib}]
    [:input#article-bib.margin-toggle {:type :checkbox}]
    [:span.sidenote "Reich, O., Shalev, G., and Kalvari, T. 2020. "
     "Modeling COVID-19 on a network: super-spreaders, testing and containment. "
     [:a {:href "https://dx.doi.org/10.1101/2020.04.30.20081828"}
      "https://dx.doi.org/10.1101/2020.04.30.20081828"] "."]
    " or "
    [:a {:href "https://github.com/ofir-reich/seir-graph"}
     "read the original python code "]
    "or "
    [:a {:href "https://github.com/aviad/covid19-seir-sim-app/issues"}
     "open an issue on github for this webpage"]
    "."]])

(defnp simulation
  []
  [:section#simulation
   [:h2 "Running the simulation"]
   [:p [:span.newthought "To run this simulation"]
    " simply adjust the parameters, and press the \"START!\" button."
    " This will cause a " [:strong "unique"]
    " random graph to be generated on your browser, and a random simulation "
    "will take place. The simulation state will be displayed on updating "
    "graphs in the " [:a {:href "#Results"} "results section"]]
   [:h3 "Adjustable simulation parameters"]
   [:div [:input.slider {:type :range :min 500 :max 20000
                         :value @(rf/subscribe [:n-nodes])
                         :step 10
                         :on-change #(rf/dispatch
                                      [:n-nodes-change (-> % .-target .-value)])
                         }]
    "Number of nodes: " @(rf/subscribe [:n-nodes])]
   [:div [:input {:type :range :min 0.1 :max 1 :name "slider"
                  :value @(rf/subscribe [:gamma])
                  :step 0.01
                  :on-change #(rf/dispatch
                               [:gamma-change (-> % .-target .-value)])}]
    "Gamma: " @(rf/subscribe [:gamma])]
   [:div [:input {:type :range :min 2 :max 30 :name "slider"
                  :value @(rf/subscribe [:mean-degree])
                  :step 1
                  :on-change #(rf/dispatch
                               [:mean-degree-change (-> % .-target .-value)])}]
    "Mean degree: " @(rf/subscribe [:mean-degree])]
   [:div [:input {:type :range :min 0 :max 1 :name "slider"
                  :value @(rf/subscribe [:prob-tested-daily])
                  :step 0.01
                  :on-change #(rf/dispatch
                               [:prob-tested-daily-change (-> % .-target .-value)])}]
    "Daily testing probability: " @(rf/subscribe [:prob-tested-daily])]
   [:div
    [:input {:type :checkbox :name "test-symptomatic"
             :checked @(rf/subscribe [:test-symptomatic])
             :on-change #(rf/dispatch
                          [:test-symptomatic-change (-> % .-target .-checked)])}]
    [:label {:for "test-symptomatic"} "Test only symptomatic"]]
   [:div
    [:button
     {:on-click #(rf/dispatch [:start-simulation])
      ;; #(profile {} (covid19.simulation/benchmark-simulation))
      }
     "START!"]]
   [hidden-run-simulation]])

(defn results
  []
  [:section#Results
   [:h2 "Results"]
   [:figure.fullwidth
    [:div#embed-lines]
    [:div#embed-bars]
    [:div#after-embed
     (when-let [history (:history @(rf/subscribe [:sim-state]))]
       (when (or (>= (count history) 300)
                 (zero? (mod (count history) 10)))
         (vega/embed
          "#embed-bars",
          (covid19.plot-sim/vglite-stacked-bar (flatten history)))
         (vega/embed
          "#embed-lines",
          (covid19.plot-sim/vglite-2-row-line-charts
           (flatten history)
           @(rf/subscribe [:n-nodes]))))
       (str "STEP: " (dec (count history))))]]])

(defn epilogue
  []
  [:section#epilogue
   [:h2 "Epilogue"]
   [:p [:span.newthought "This webpage was created "]
    "with "
    [:a {:href "https://clojurescript.org/"} "clojurescript"]
    ", "
    [:a {:href "https://github.com/Day8/re-frame"} "re-frame"]
    ", "
    [:a {:href "https://vega.github.io/vega-lite/"} "vega-lite"]
    ", "
    [:a {:href "https://shadow-cljs.org/"} "shadow-cljs"]
    " and "
    [:a {:href "https://github.com/Day8/re-frame"} "tufte-css"]
    "."]
   [:p "It brought me joy."]])

(defn ui
  []
  [:article
   [:h1 "Modeling COVID-19 on a network: super-spreaders, testing and containment"]
   [:p.subtitle "In-browser demo of the "
    [:a {:href "https://www.medrxiv.org/content/10.1101/2020.04.30.20081828v1"
         :target "_blank"
         :rel "noreferrer noopener"}
     "article by Reich et. al."]]
   [general-intro]
   [simulation]
   [results]
   [epilogue]])


;; -- Entry Point -------------------------------------------------------------

(defn render
  []
  (dom/render [ui] (js/document.getElementById "app")))

(defn ^:dev/after-load clear-cache-and-render!
  []
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code. We force a UI update by clearing
  ;; the Reframe subscription cache.
  (rf/clear-subscription-cache!)
  (render))

(defn run
  []
  (rf/dispatch-sync [:initialize]) ;; put a value into application state
  (render))                        ;; mount the application's ui into '<div id="app" />'
