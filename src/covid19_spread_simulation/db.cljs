(ns covid19-spread-simulation.db)

(def default-db
  {:n-nodes 5000
   :gamma 0.2
   :mean-degree 20
   :sim-state nil
   :test-symptomatic? false
   :mean-days-to-detect-infected 5
   :test-everyone? false
   :tests-per-1m-people 400
   :saved-simulations []})
