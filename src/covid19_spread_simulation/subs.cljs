(ns covid19-spread-simulation.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::n-nodes
 (fn [db _]
   (:n-nodes db)))

(re-frame/reg-sub
 ::gamma
 (fn [db _]
   (:gamma db)))

(re-frame/reg-sub
 ::mean-degree
 (fn [db _]
   (:mean-degree db)))

(re-frame/reg-sub
 ::test-symptomatic?
 (fn [db _]
   (:test-symptomatic? db)))

(re-frame/reg-sub
 ::mean-days-to-detect-infected
 (fn [db _]
   (:mean-days-to-detect-infected db)))

(re-frame/reg-sub
 ::test-everyone?
 (fn [db _]
   (:test-everyone? db)))

(re-frame/reg-sub
 ::tests-per-1m-people
 (fn [db _]
   (:tests-per-1m-people db)))

(re-frame/reg-sub
 ::sim-state
 (fn [db _]
   (:sim-state db)))

(re-frame/reg-sub
 ::start-simulation
 (fn [db _]
   (:start-simulation db)))

(re-frame/reg-sub
 ::saved-simulations
 (fn [db _]
   (:saved-simulations db)))
