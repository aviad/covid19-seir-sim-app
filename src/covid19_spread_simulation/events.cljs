(ns covid19-spread-simulation.events
  (:require
   [re-frame.core :as re-frame]
   [covid19-spread-simulation.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   [clojure.edn :as edn]
   [covid19-spread-simulation.simulation :refer [initialize]]
   ))

(re-frame/reg-event-db ;; sets up initial application state
 ::initialize-db       ;; usage:  (dispatch [:initialize-db])
 (fn-traced            ;; the two parameters are not important here, so use _
  [_ _]                ;; What it returns becomes the new application state
  (if-let [stored-db js/window.sessionStorage.db]
    (edn/read-string stored-db)
    db/default-db)))

(re-frame/reg-event-db
 ::save-to-session-storage
 (fn [db]
   (when db
     ;; do not save currently running simulation. graph is 1-5 MBs
     (set! js/window.sessionStorage.db (pr-str (assoc db :sim-state nil))))
   db))

(re-frame/reg-event-db                ;; usage:  (dispatch [:n-nodes-change 0.123])
 ::n-nodes-change
 (fn [db [_ new-value]]
   (assoc db :n-nodes new-value)))

(re-frame/reg-event-db
 ::gamma-change
 (fn [db [_ new-value]]
   (assoc db :gamma new-value)))

(re-frame/reg-event-db
 ::mean-degree-change
 (fn [db [_ new-value]]
   (assoc db :mean-degree new-value)))

(re-frame/reg-event-db
 ::mean-days-to-detect-infected-change
 (fn [db [_ new-value]]
   (assoc db :mean-days-to-detect-infected new-value)))

(re-frame/reg-event-db
 ::tests-per-1m-people-change
 (fn [db [_ new-value]]
   (assoc db :tests-per-1m-people new-value)))

(re-frame/reg-event-db
 ::test-symptomatic-change
 (fn [db [_ new-value]]
   (assoc db :test-symptomatic? new-value)))

(re-frame/reg-event-db
 ::test-everyone-change
 (fn [db [_ new-value]]
   (assoc db :test-everyone? new-value)))

(re-frame/reg-event-db
 ::start-simulation
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

(re-frame/reg-event-db
 ::sim-state-change
 (fn [db [_ new-state]]
   (update db :sim-state merge new-state)))

(re-frame/reg-event-db
 ::save-simulation
 (fn [db [_ sim-state]]
   (-> db
       (update :saved-simulations conj (dissoc sim-state :g))
       (assoc :sim-state nil))))

(re-frame/reg-event-db
 ::clear-results
 (fn [db [_ _]]
   (assoc db :saved-simulations [])))
