(ns covid19-spread-simulation.views
  (:require
   [re-frame.core :as re-frame]
   [covid19-spread-simulation.events :as events]
   [covid19-spread-simulation.subs :as subs]
   [covid19-spread-simulation.plot-sim :as plot-sim]
   [covid19-spread-simulation.simulation :as simulation]
   ["vega-embed" :as vega]))

(defn hidden-run-simulation
  []
  (when-let [sim-state @(re-frame/subscribe [::subs/sim-state])]
    (when-let [history (:history sim-state)]
      (if (>= 300 (count history))
        (let [new-state (simulation/simulation-step (:g sim-state))
              step-results (flatten (map assoc
                                         (simulation/graph->counts new-state)
                                         (repeat :step)
                                         (repeat (count history))))
              updated-history (conj history step-results)]
          (re-frame/dispatch [::events/sim-state-change
                              {:g new-state :history updated-history}]))
        (re-frame/dispatch [::events/save-simulation sim-state])))))

(defn introduction
  []
  [:section#introduction
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
                         :value @(re-frame/subscribe [::subs/n-nodes])
                         :step 10
                         :on-change #(re-frame/dispatch
                                      [::events/n-nodes-change (-> % .-target .-value)])}]
    "Number of nodes: " @(re-frame/subscribe [::subs/n-nodes])]
   [:div [:input {:type :range :min 0.1 :max 1 :name "slider"
                  :value @(re-frame/subscribe [::subs/gamma])
                  :step 0.01
                  :on-change #(re-frame/dispatch
                               [::events/gamma-change (-> % .-target .-value)])}]
    "Gamma: " @(re-frame/subscribe [::subs/gamma])]
   [:div [:input {:type :range :min 2 :max 30 :name "slider"
                  :value @(re-frame/subscribe [::subs/mean-degree])
                  :step 1
                  :on-change #(re-frame/dispatch
                               [::events/mean-degree-change (-> % .-target .-value)])}]
    "Mean degree: " @(re-frame/subscribe [::subs/mean-degree])]
   [:div
    [:input {:type :checkbox :id "test-symptomatic"
             :checked @(re-frame/subscribe [::subs/test-symptomatic?])
             :on-change #(re-frame/dispatch
                          [::events/test-symptomatic-change (-> % .-target .-checked)])}]
    [:label {:for "test-symptomatic"} "Test symptomatic?"]]
   (let [possibly-greyed-div (if @(re-frame/subscribe [::subs/test-symptomatic?])
                               :div
                               :div.greyedOut)]
     [possibly-greyed-div
      [:input {:type :range :min 0 :max 25 :name "slider"
               :value @(re-frame/subscribe [::subs/mean-days-to-detect-infected])
               :step 0.5
               :on-change #(when @(re-frame/subscribe [::subs/test-symptomatic?])
                             (re-frame/dispatch
                              [::events/mean-days-to-detect-infected-change
                               (-> % .-target .-value)]))}]
      "Means days for infection detection: " @(re-frame/subscribe [::subs/mean-days-to-detect-infected])])
   [:div
    [:input {:type :checkbox :id "test-everyone"
             :checked @(re-frame/subscribe [::subs/test-everyone?])
             :on-change #(re-frame/dispatch
                          [::events/test-everyone-change (-> % .-target .-checked)])}]
    [:label {:for "test-everyone"} "Test general population?"]]
   (let [possibly-greyed-div (if @(re-frame/subscribe [::subs/test-everyone?])
                               :div
                               :div.greyedOut)]
     [possibly-greyed-div
      [:input {:type :range :min 0 :max 100000 :name "slider"
               :value @(re-frame/subscribe [::subs/tests-per-1m-people])
               :step 10
               :on-change #(when @(re-frame/subscribe [::subs/test-everyone?])
                             (re-frame/dispatch
                              [::events/tests-per-1m-people-change (-> % .-target .-value)]))}]
      "Daily tests per 1M people: "
      @(re-frame/subscribe [::subs/tests-per-1m-people])])
   [:p
    ""
    [:input.margin-toggle {:type :checkbox :id :icon-credit}]
    [:label.margin-toggle {:for :icon-credit} "&#8853;"]
    [:span.marginnote
     "\"send\" icon by "
     [:a {:href "https://www.brandeps.com/"} "https://www.brandeps.com/"]
     ", licensed under "
     [:a {:href "https://creativecommons.org/licenses/by-sa/4.0/"}
      "https://creativecommons.org/licenses/by-sa/4.0/"]]
    [:span#run-sim-button.myButton
     {:on-click #(re-frame/dispatch [::events/start-simulation])
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
           (covid19-spread-simulation.plot-sim/vglite-stacked-bar history))
          (vega/embed
           embed-lines-id
           (covid19-spread-simulation.plot-sim/vglite-2-row-line-charts history n-nodes))
          "")]]])])

(defn results
  []
  [:section#results
   [:h2 "Results "]
   [:div [:a {:href "#Results" :on-click #(re-frame/dispatch [::events/clear-results])} "clear all results"]]
   ;; Currently running simulation
   (when-let [sim-state @(re-frame/subscribe [::subs/sim-state])]
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
             (covid19-spread-simulation.plot-sim/vglite-2-row-line-charts
              (flatten history)
              @(re-frame/subscribe [::subs/n-nodes])))
            (vega/embed
             "#embed-bars-running",
             (covid19-spread-simulation.plot-sim/vglite-stacked-bar (flatten history))))
          (str "STEP: " (dec (count history))))]]])
   [saved-simulations @(re-frame/subscribe [::subs/saved-simulations])]])

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
   [introduction]
   [simulation]
   [results]
   [epilogue]])
