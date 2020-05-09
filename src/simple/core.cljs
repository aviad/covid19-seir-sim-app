(ns simple.core
  (:require [reagent.dom :as dom]
            [re-frame.core :as rf]
            [clojure.string :as str]
            ["jstat" :as jstat]
            ["vega-embed" :as vega]
            [simple.plot-geom :as plot-geom]
            [simple.plot-sim :as plot-sim]
            [simple.simulation :refer [initialize simulation-step graph->counts]]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            ))

;; A detailed walk-through of this source code is provided in the docs:
;; https://github.com/day8/re-frame/blob/master/docs/CodeWalkthrough.md

;; -- Utility funcs -----------------------------------------------------------
(defn- game-won?
  [board]
  (let [lines [[0 1 2] [3 4 5] [6 7 8]
               [0 3 6] [1 4 7] [2 5 8]
               [0 4 8] [2 4 6]]]
    (first
     (some #{"XXX" "OOO"}
           (for [line lines]
             (str/join (map board line)))))))

(defn- player
  [step]
  (if (zero? (mod step 2)) "X" "O"))
;; -- Domino 1 - Event Dispatch -----------------------------------------------

(defn dispatch-timer-event
  []
  (let [now (js/Date.)]
    (rf/dispatch [:timer now])))  ;; <-- dispatch used

;; Call the dispatching function every second.
;; `defonce` is like `def` but it ensures only one instance is ever
;; created in the face of figwheel hot-reloading of this file.
(defonce do-timer (js/setInterval dispatch-timer-event 1000))


;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db              ;; sets up initial application state
  :initialize                 ;; usage:  (dispatch [:initialize])
  (fn [_ _]                   ;; the two parameters are not important here, so use _
    {:time (js/Date.)         ;; What it returns becomes the new application state
     :time-color "HOTPINK"
     :history [(vec (take 9 (repeat "")))]
     :step-number 0
     :graph-const (rand)
     :n-nodes 5000
     :gamma 0.2
     :mean-degree 20
     :sim-state nil
     :share-tested 0.3
     :test-symptomatic true}))


(rf/reg-event-db                ;; usage:  (dispatch [:time-color-change 34562])
  :time-color-change            ;; dispatched when the user enters a new colour into the UI text field
  (fn [db [_ new-color-value]]  ;; -db event handlers given 2 parameters:  current application state and event (a vector)
    (assoc db :time-color new-color-value)))   ;; compute and return the new application state

(rf/reg-event-db                 ;; usage:  (dispatch [:timer a-js-Date])
 :timer                         ;; every second an event of this kind will be dispatched
 (fn [db [_ new-time]]          ;; note how the 2nd parameter is destructured to obtain the data value
   (assoc db :time new-time)))  ;; compute and return the new application state

(rf/reg-event-db  ;; usage:  (dispatch [:square-clicked 5])
 :square-clicked  ;; dispatched when the user clicks a square
 (fn [db [_ i]]   ;; -db event handlers given 2 parameters:  current application state and event (a vector)
   (let [{:keys [step-number history]} db
         history (subvec (:history db) 0 (inc step-number))
         board-before (last history)]
     (if (or (not-empty (board-before i))
             (game-won? board-before))
       db                               ; "illegal" move - no change
       (assoc db ;; compute and return the new application state
              :history (conj history
                             (assoc board-before i
                                    (player step-number)))
              :step-number (inc step-number))))))

(rf/reg-event-db
 :jump-to
 (fn [db [_ step]]
   (assoc db
          :step-number step)))

(rf/reg-event-db                ;; usage:  (dispatch [:graph-const-change 0.123])
 :graph-const-change
 (fn [db [_ new-const-value]]
   (assoc db :graph-const new-const-value)))

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
 :share-tested-change
 (fn [db [_ new-value]]
   (assoc db :share-tested new-value)))

(rf/reg-event-db
 :test-symptomatic-change
 (fn [db [_ new-value]]
   (assoc db :test-symptomatic new-value)))

(rf/reg-event-db
 :start-simulation
 (fn [db [_ _]]
   ;; (prn :start-simulation :reg-event-db)
   (let [{:keys [n-nodes gamma mean-degree share-tested test-symptomatic]} db
         initial-graph (initialize {:n-nodes n-nodes
                                    :gamma gamma
                                    :mean-degree mean-degree
                                    :share-tested share-tested
                                    :test-symptomatic test-symptomatic})
         ;; _ (prn "starting simulation")
         ]
    (assoc db :sim-state {:g initial-graph :history []}))))

(rf/reg-event-db
 :sim-state-change
 (fn [db [_ new-state]]
   (assoc db :sim-state new-state)))

;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
  :time
  (fn [db _]     ;; db is current app state. 2nd unused param is query vector
    (:time db))) ;; return a query computation over the application state

(rf/reg-sub
  :time-color
  (fn [db _]
    (:time-color db)))

(rf/reg-sub
 :board
 (fn [db _]
   (get (:history db) (:step-number db))))

(rf/reg-sub
 :history
 (fn [db _]
   (:history db)))

(rf/reg-sub
 :step-number
 (fn [db _]
   (:step-number db)))

(rf/reg-sub
 :graph-const
 (fn [db _]
   (:graph-const db)))

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
 :share-tested
 (fn [db _]
   (:share-tested db)))

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

(defn clock
  []
  [:div.example-clock
   {:style {:color @(rf/subscribe [:time-color])}}
   (-> @(rf/subscribe [:time])
       .toTimeString
       (str/split " ")
       first)])

(defn color-input
  []
  [:div.color-input
   "Time color: "
   [:input {:type "text"
            :value @(rf/subscribe [:time-color])
            :on-change #(rf/dispatch [:time-color-change (-> % .-target .-value)])}]])  ;; <---

;; tic tac toe
(defn square
  ^{:id (str "square" i)}
  [i]
  [:button.square
   {:key i
    :on-click #(rf/dispatch [:square-clicked i])}
   (get @(rf/subscribe [:board]) i)])

(defn board
  []
  [:div
   (for [row (range 3)
         :let [start (* row 3)
               end (+ 3 start)]]
     ^{:key (str "row" row)}
     [:div.board-row (for [i (range start end)]
                       ^{:key (str "square-" i)} [square i])])])

(defn status
  []
  (if-let [winner (game-won? @(rf/subscribe [:board]))]
    (str "Winner: " winner)
    (str "Next player: " (player @(rf/subscribe [:step-number])))))

(defn moves
  []
  [:ol
   (for [move (range (count @(rf/subscribe [:history])))]
     ^{:key (str "move" move)}
     [:li [:button
           {:on-click #(rf/dispatch [:jump-to move])}
           (if (zero? move)
             "Go to game start"
             (str "Go to move #" move))]])])

(defn tic-tac-toe
  []
  [:div.game
   [:div.game-board
    [board]
    ]
   [:div.game-info
    [:div
     [status]]
    [moves]]])

;; showing a graph
(defn svg
  []
  [:div
   ;; [:div plot-geom/svg]
   [:div
    [:div @(rf/subscribe [:graph-const])]
    [:div [:input {:type :range :min 0 :max 1 :name "slider"
             :value @(rf/subscribe [:graph-const])
             :step 0.01
             :on-change #(rf/dispatch
                          [:graph-const-change (-> % .-target .-value)])}]
     "Constant"]
    ]
   [:div (plot-geom/gen-svg @(rf/subscribe [:graph-const]))]])

;; random changing gamma-sampled value
(defn gamma-dist-sample
  []
  [:div
   (str (and @(rf/subscribe [:time]) "") (jstat/gamma.sample 1 1))])




;; (def vglite-lines
;;   #js {"$schema" "https://vega.github.io/schema/vega-lite/v4.json", :description "Stock prices of 5 Tech Companies over Time.", :data #js {:url "https://raw.githubusercontent.com/vega/vega-lite-v1/master/data/stocks.csv"}, :mark "line", :encoding #js {:x #js {:field "date", :type "temporal"}, :y #js {:field "price", :type "quantitative"}, :color #js {:field "symbol", :type "nominal"}}}
;; )

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
          (simple.plot-sim/vglite-stacked-bar (flatten history)))
         (vega/embed
          "#embed-lines",
          (simple.plot-sim/vglite-2-row-line-charts
           (flatten history)
           @(rf/subscribe [:n-nodes]))))
       (str "STEP: " (dec (count history))))]]])


(defn general-intro
  []
  [:section#general-intro
   [:h2 "Introduction"]
   [:p [:span.newthought "The article has an agent-based simulation"]
    " of COVID-19 on a human-like connections network. This page allows you "
    "to experiment with this simulation, adjusting some of its parameters. "
    "It is creating a network just like the one in the "
    [:a {:href "https://www.medrxiv.org/content/10.1101/2020.04.30.20081828v1"} "article"]
    ", and simulates the disease spreading on that network."]
   [:p "For details, read the article"
    [:label.margin-toggle.sidenote-number {:for :article-bib}]
    [:input#article-bib.margin-toggle {:type :checkbox}]
    [:span.sidenote "Reich, O., Shalev, G., and Kalvari, T. 2020. Modeling COVID-19 on a network: super-spreaders, testing and containment. http://dx.doi.org/10.1101/2020.04.30.20081828."]
    " or open an issue on Github."]
   ])

(profile
 {}
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
                                       [:n-nodes-change (-> % .-target .-value)])}]
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
                   :value @(rf/subscribe [:share-tested])
                   :step 0.01
                   :on-change #(rf/dispatch
                                [:share-tested-change (-> % .-target .-value)])}]
     "Share tested: " @(rf/subscribe [:share-tested])]
    [:div
     [:input {:type :checkbox :name "test-symptomatic"
              :checked @(rf/subscribe [:test-symptomatic])
              :on-change #(rf/dispatch
                           [:test-symptomatic-change (-> % .-target .-checked)])}]
     [:label {:for "test-symptomatic"} "Test only symptomatic"]]
    [:div
     [:button
      {:on-click #(rf/dispatch [:start-simulation])
       ;; #(profile {} (simple.simulation/benchmark-simulation))
       }
      "START!"]]
    [hidden-run-simulation]]))

(defn ui
  []
  [:article
   [:h1 "Modeling COVID-19 on a network: super-spreaders, testing and containment"]
   [:p.subtitle "In-browser demo of the " [:a {:href "https://www.medrxiv.org/content/10.1101/2020.04.30.20081828v1"} "article"] " by Reich et. al."]
   [general-intro]
   [simulation]
   [results]])

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
  (render)                         ;; mount the application's ui into '<div id="app" />'
  )

(tufte/add-basic-println-handler! {})
