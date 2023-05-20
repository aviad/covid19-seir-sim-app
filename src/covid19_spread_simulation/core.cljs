(ns covid19-spread-simulation.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as re-frame]
   [covid19-spread-simulation.events :as events]
   [covid19-spread-simulation.views :as views]
   [covid19-spread-simulation.config :as config]))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    ;; (rdom/render [views/main-panel] root-el)
    (rdom/render [views/ui] root-el)))

(defn init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))

(defn save-state-to-session-storage
  "This is to keep the data across refreshing the page."
  []
  (re-frame/dispatch [::events/save-to-session-storage]))

;; Call the dispatching function every 5 seconds.
;; `defonce` is like `def` but it ensures only one instance is ever
;; created in the face of figwheel hot-reloading of this file.
(defonce save-state-periodically
  (js/setInterval save-state-to-session-storage 5000))
