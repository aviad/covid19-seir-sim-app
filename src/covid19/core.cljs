(ns covid19.core
  (:require [reagent.dom :as dom]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [clojure.edn :as edn]
            ["jstat" :as jstat]
            ["vega-embed" :as vega]
            [covid19.plot-geom :as plot-geom]
            [covid19.plot-sim :as plot-sim]
            [covid19.simulation :refer [initialize simulation-step graph->counts]]
            ;; [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            ))

;; -- Domino 1 - Event Dispatch -----------------------------------------------
(defn save-state-to-session-storage
  "This is to keep the data across refreshing the page."
  []
  (rf/dispatch [:save-to-session-storage]))

;; Call the dispatching function every 5 seconds.
;; `defonce` is like `def` but it ensures only one instance is ever
;; created in the face of figwheel hot-reloading of this file.
(defonce save-state-periodically
  (js/setInterval save-state-to-session-storage 5000))

;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db              ;; sets up initial application state
  :initialize                 ;; usage:  (dispatch [:initialize])
  (fn [_ _]                   ;; the two parameters are not important here, so use _
                              ;; What it returns becomes the new application state
    (if-let [stored-db js/window.sessionStorage.db]
      (edn/read-string stored-db)
      {:n-nodes 5000
       :gamma 0.2
       :mean-degree 20
       :sim-state nil
       :test-symptomatic? false
       :mean-days-to-detect-infected 5
       :test-everyone? false
       :tests-per-1m-people 400
       :saved-simulations []})))

(rf/reg-event-db
 :save-to-session-storage
 (fn [db]
   (when db
     ;; do not save currently running simulation. graph is 1-5 MBs
     (set! js/window.sessionStorage.db (pr-str (assoc db :sim-state nil))))
   db))

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
 :mean-days-to-detect-infected-change
 (fn [db [_ new-value]]
   (assoc db :mean-days-to-detect-infected new-value)))

(rf/reg-event-db
 :tests-per-1m-people-change
 (fn [db [_ new-value]]
   (assoc db :tests-per-1m-people new-value)))

(rf/reg-event-db
 :test-symptomatic-change
 (fn [db [_ new-value]]
   (assoc db :test-symptomatic? new-value)))

(rf/reg-event-db
 :test-everyone-change
 (fn [db [_ new-value]]
   (assoc db :test-everyone? new-value)))

(rf/reg-event-db
 :start-simulation
 (fn [db [_ _]]
   (let [{:keys [test-symptomatic? test-everyone?]} db
         ;; if not testing symptomatic, this means an infected node will never
         ;; be detected, so remove from the parameters. Same goes for testing
         ;; of the general population
         simulation-parameters (select-keys db
                                            [:n-nodes :gamma :mean-degree
                                             :test-symptomatic?
                                             (when test-symptomatic? :mean-days-to-detect-infected)
                                             :test-everyone?
                                             (when test-everyone? :tests-per-1m-people)])
         initial-graph (initialize simulation-parameters)]
     (assoc db :sim-state
            (merge simulation-parameters {:g initial-graph :history []})))))

(rf/reg-event-db
 :sim-state-change
 (fn [db [_ new-state]]
   (update db :sim-state merge new-state)))

(rf/reg-event-db
 :save-simulation
 (fn [db [_ sim-state]]
   (-> db
       (update :saved-simulations conj (dissoc sim-state :g))
       (assoc :sim-state nil))))

(rf/reg-event-db
 :clear-results
 (fn [db [_ _]]
   (assoc db :saved-simulations [])))

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
 :test-symptomatic?
 (fn [db _]
   (:test-symptomatic? db)))

(rf/reg-sub
 :mean-days-to-detect-infected
 (fn [db _]
   (:mean-days-to-detect-infected db)))

(rf/reg-sub
 :test-everyone?
 (fn [db _]
   (:test-everyone? db)))

(rf/reg-sub
 :tests-per-1m-people
 (fn [db _]
   (:tests-per-1m-people db)))

(rf/reg-sub
 :sim-state
 (fn [db _]
   (:sim-state db)))

(rf/reg-sub
 :start-simulation
 (fn [db _]
   (:start-simulation db)))

(rf/reg-sub
 :saved-simulations
 (fn [db _]
   (:saved-simulations db)))

;; -- Domino 5 - View Functions ----------------------------------------------

(defn hidden-run-simulation
  []
  (when-let [sim-state @(rf/subscribe [:sim-state])]
    (when-let [history (:history sim-state)]
      (if (>= 300 (count history))
        (let [new-state (simulation-step (:g sim-state))
              step-results (flatten (map assoc
                                         (graph->counts new-state)
                                         (repeat :step)
                                         (repeat (count history))))
              updated-history (conj history step-results)]
          (rf/dispatch [:sim-state-change
                        {:g new-state :history updated-history}]))
        (rf/dispatch [:save-simulation sim-state])))))

(defn general-intro
  []
  [:section#general-intro
   [:h2 "Introduction"]
   [:p [:span.newthought "The article"]
    [:label.margin-toggle.sidenote-number {:for :article-bib}]
    [:input#article-bib.margin-toggle {:type :checkbox}]
    [:span.sidenote "Reich, O., Shalev, G., and Kalvari, T. 2020. "
     "Modeling COVID-19 on a network: super-spreaders, testing and containment. "
     [:a {:href "https://dx.doi.org/10.1101/2020.04.30.20081828"}
      "https://dx.doi.org/10.1101/2020.04.30.20081828"] "."]
    [:span.newthought " models the spread of covid-19"]
    " using an SEIR agent-based model on a graph, taking into account several important real-life attributes of covid-19: Super-spreaders, realistic epidemiological parameters of the disease, testing and quarantine policies. The findings indicate that mass-testing is much less effective than testing the symptomatic and contact tracing, and some blend of these with social distancing is required to get suppression."]
   [:p "This page allows you to experiment with this simulation, adjusting some of its parameters. "
    "It is creating a " [:strong "unique"] " network just like the one in the "
    [:a {:href "https://www.medrxiv.org/content/10.1101/2020.04.30.20081828v1"
         :target "_blank"
         :rel "noreferrer noopener"}
     "article"]
    ", and simulates the disease spreading on that network. "]
   [:p "Try running the simulation several times with the same parameters, to witness the similarities and differences due to randominzation. Then, try adjusting the parameters to investigate different strategies and see their effect on disease spread, seeing which ones lead to containment, and which ones just infect a substantial share of the population."]
   [:p "For details, read the article, or "
    [:a {:href "https://github.com/ofir-reich/seir-graph"}
     "read the original python code"]
    ", or "
    [:a {:href "https://github.com/aviad/covid19-seir-sim-app/issues"}
     "open an issue on github for this webpage"]
    "."]])

(defn simulation
  []
  [:section#simulation
   [:h2 "Running the simulation"]
   [:p [:span.newthought "To run this simulation"]
    " simply adjust the parameters, and press the "
    [:a.monospaced {:href "#adjusting"} "Run"]
    " button below."
    " This will cause a " [:strong "unique"]
    " random graph to be generated on your browser, and a random simulation "
    "will take place. The simulation state will be displayed on updating "
    "graphs in the " [:a {:href "#Results"} "results section"]
    ". Scroll down to see previous simulations you ran."]
   [:h3#adjusting "Adjustable simulation parameters"]
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
   [:div
    [:input {:type :checkbox :name "test-symptomatic"
             :checked @(rf/subscribe [:test-symptomatic?])
             :on-change #(rf/dispatch
                          [:test-symptomatic-change (-> % .-target .-checked)])}]
    [:label {:for "test-symptomatic"} "Test symptomatic?"]]
   (let [possibly-greyed-div (if @(rf/subscribe [:test-symptomatic?])
                               :div
                               :div.greyedOut)]
     [possibly-greyed-div
      [:input {:type :range :min 0 :max 25 :name "slider"
               :value @(rf/subscribe [:mean-days-to-detect-infected])
               :step 0.5
               :on-change #(when @(rf/subscribe [:test-symptomatic?])
                               (rf/dispatch
                                [:mean-days-to-detect-infected-change
                                 (-> % .-target .-value)]))}]
      "Means days for infection detection: " @(rf/subscribe [:mean-days-to-detect-infected])])
   [:div
    [:input {:type :checkbox :name "test-everyone"
             :checked @(rf/subscribe [:test-everyone?])
             :on-change #(rf/dispatch
                          [:test-everyone-change (-> % .-target .-checked)])}]
    [:label {:for "test-everyone"} "Test general population?"]]
   (let [possibly-greyed-div (if @(rf/subscribe [:test-everyone?])
                               :div
                               :div.greyedOut)]
     [possibly-greyed-div
      [:input {:type :range :min 0 :max 100000 :name "slider"
               :value @(rf/subscribe [:tests-per-1m-people])
               :step 10
               :on-change #(when @(rf/subscribe [:test-everyone?])
                            (rf/dispatch
                             [:tests-per-1m-people-change (-> % .-target .-value)]))}]
      "Daily tests per 1M people: "
      @(rf/subscribe [:tests-per-1m-people])])



   [:p
    ""
    [:label.margin-toggle {:for :icon-credit} "&#8853;"]
    [:input.margin-toggle {:type :checkbox :id :icon-credit}]
    [:span.marginnote
     "\"send\" icon by "
     [:a {:href "https://www.brandeps.com/"} "https://www.brandeps.com/"]
     ", licensed under "
     [:a {:href "https://creativecommons.org/licenses/by-sa/4.0/"}
      "https://creativecommons.org/licenses/by-sa/4.0/"]]
    [:span#run-sim-button.myButton
     {:on-click #(rf/dispatch [:start-simulation])
      :href "#Results"
      :height "19em"}
     "Run "
     [:img.send-icon
      {:src "https://metta-code.s3.eu-central-1.amazonaws.com/send.svg"
       :height "26em"}]]]
   [hidden-run-simulation]])

(defn params-table
  [params]
  [:table
   [:thead
    [:tr [:th "Parameter"] [:th [:span.invisible "..."]] [:th "Value"]]]
   [:tbody
    [:tr [:td "Number of nodes"] [:td] [:td (:n-nodes params)]]
    [:tr [:td "Gamma"] [:td] [:td (:gamma params)]]
    [:tr [:td "Mean degree"] [:td] [:td (:mean-degree params)]]
    [:tr [:td "Test symptomatic"] [:td] [:td (str (:test-symptomatic? params))]]
    (when (:test-symptomatic? params)
      [:tr [:td "Means days for infection detection"] [:td] [:td (:mean-days-to-detect-infected params)]])
    [:tr [:td "Test general population"] [:td] [:td (str (:test-everyone? params))]]
    (when (:test-everyone? params)
      [:tr [:td "No. of daily tests per 1M people"] [:td] [:td (:tests-per-1m-people params)]])]])

(defn saved-simulations
  [sims]
  [:div
   (for [sim-i (reverse (range (count sims)))
         :let [sim (sims sim-i)
               history (flatten (:history sim))
               n-nodes (:n-nodes sim)
               display-idx (inc sim-i)
               embed-lines-id (str "#embed-lines-" display-idx)
               embed-bars-id (str "#embed-bars-" display-idx)]]
     ^{:key (str "saved-simulation-" display-idx)}
     [:div
      [:h3 {:id (str "Simulation-" display-idx)} "Simulation " display-idx]
      (params-table sim)
      [:figure.fullwidth
       [(keyword (str "div" embed-lines-id))]
       [(keyword (str "div" embed-bars-id))]
       [(keyword (str "div#do-embed-" display-idx))
        (do
          (vega/embed
           embed-bars-id
           (covid19.plot-sim/vglite-stacked-bar history))
          (vega/embed
           embed-lines-id
           (covid19.plot-sim/vglite-2-row-line-charts history n-nodes))
          "")]]])])

(defn results
  []
  [:section#Results
   [:h2 "Results "]
   [:div [:a {:href "#Results" :on-click #(rf/dispatch [:clear-results])} "clear all results"]]
   ;; Currently running simulation
   (when-let [sim-state @(rf/subscribe [:sim-state])]
     [:div
      [:h3#running-simulation "Running simulation"]
      [params-table (:g sim-state)]
      [:figure.fullwidth
       [:div#embed-lines-running]
       [:div#embed-bars-running]
       [:div#after-embed
        (when-let [history (:history sim-state)]
          (when (or (= (count history) 301)
                    (zero? (mod (count history) 10)))
            (vega/embed
             "#embed-lines-running",
             (covid19.plot-sim/vglite-2-row-line-charts
              (flatten history)
              @(rf/subscribe [:n-nodes])))
            (vega/embed
             "#embed-bars-running",
             (covid19.plot-sim/vglite-stacked-bar (flatten history))))
          (str "STEP: " (dec (count history))))]]])
   [saved-simulations @(rf/subscribe [:saved-simulations])]])

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
   [:p.subtitle "Simulate COVID-19 spread according to the "
    [:a {:href "https://www.medrxiv.org/content/10.1101/2020.04.30.20081828v1"
         :target "_blank"
         :rel "noreferrer noopener"}
     "article by Reich et. al."]]
   [:p [:strong "TL;DR: " [:a {:href "#simulation"} "Go straight to running the simulation"]]]
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
