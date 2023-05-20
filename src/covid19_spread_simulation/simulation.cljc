(ns covid19-spread-simulation.simulation
  (:require [loom.attr]
            [loom.graph :refer [nodes]]
            #?(:cljs ["jstat" :as jstat]
               :clj [incanter.distributions :as dist])
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; To understand what is going on, start from `simulation-step` and `initialize`

(defnp gen-neighbors
  [node all-nodes degree]
  ;; ensure all-nodes support random-access
  {:pre [(vector? all-nodes)]}
  (let [neighbors (p :rand-nth (repeatedly degree #(rand-nth all-nodes)))]
    (for [neighbor neighbors] [node neighbor])))

(defn- mean
  [& nums]
  (/ (reduce + nums) (count nums)))

(defn- gen-degrees
  "X = (1 / (random ^ gamma)) - 1
  M = mean(X)
  Y = X / M
  ; divided mean_degree by 2 since this is an undirected graph
  Z = min_degree + Y * ((mean_degree / 2) - min_degree)
  D = round(Z)"
  [^long nodes-n ^long mean-degree ^double gamma]
  {:pre [(<= 0 gamma 1)
         (pos? mean-degree)
         (pos? nodes-n)]
   :post [(<= 2 (apply min %))
          ;; make sure we're in the right area with actual generated degrees
          (<= (dec mean-degree) (* 2 (apply mean %)) (inc mean-degree))]}
  (let [min-degree 2                 ;; min-degree is 2 HARD-CODED
        Xs (for [_ (range nodes-n)]
             (dec (/ 1 (Math/pow (rand) gamma))))
        m (/ (reduce + Xs) (count Xs))]
    (for [x Xs
          :let [y (/ x m)
                z (+ min-degree (* y (- (/ mean-degree 2) min-degree)))]]
      (Math/round z))))

(defnp connect
  [g mean-degree gamma]
  (let [g-nodes (p :nodes-g (vec (nodes g)))
        degrees (p :gen-degrees (gen-degrees (count g-nodes) mean-degree gamma))
        edges (p :mapcat-gen-neighbors
                 (mapcat gen-neighbors g-nodes (repeat g-nodes) degrees))]
    (p :add-edges* (loom.graph/add-edges* g edges))))

(defnp incubation-time
  "generate a gamma-distributed incubation time in days"
  []
  (let [shape 3.7616470588235296
        scale 1.3557890786263838]
    #?(:cljs (jstat/gamma.sample shape scale)
       :clj (dist/draw (dist/gamma-distribution shape scale)))))

(defnp to-I
  [step]
  (p :to-I (+ step (Math/round (* 5 (incubation-time))))))

(defn- susceptible?
  [g node]
  (= :S (loom.attr/attr g node :state)))

(defn- exposed?
  [g node]
  (= :E (loom.attr/attr g node :state)))

(defn- EI?
  [g node]
  (= :EI (loom.attr/attr g node :state)))

(defn- infected?
  [g node]
  (= :I (loom.attr/attr g node :state)))

(defnp random-infect
  "randomly infect nodes"
  [g n]
  (let [susceptible-nodes (vec (nodes g))]
    (loom.attr/add-attr-to-nodes
     g
     :state :I
     (take n (distinct (repeatedly #(rand-nth susceptible-nodes)))))))

(defnp random-expose
  "randomly expose nodes"
  [g n]
  (let [susceptible-nodes (vec (filter #(susceptible? g %) (nodes g)))
        exposed (take n (distinct (repeatedly #(rand-nth susceptible-nodes))))
        g (loom.attr/add-attr-to-nodes g :state :E exposed)]
    (reduce (fn [g node]
              (loom.attr/add-attr g node :to-I (to-I 0)))
            g exposed)))

(defn- nodes-and-attrs-by-state
  [g state]
  (filter (fn [[_ v]] (= state (:state v))) (:attrs g)))

(defnp nodes-by-state
  [g state]
  (map first (nodes-and-attrs-by-state g state)))

(defnp infect
  [g sources infection-prob]
  (let [non-quarantined (filter
                         (fn [node] (nil? (loom.attr/attr g node :TP)))
                         sources)
        step (:step g)
        find-infected
        (fn [node]
          (let [neighbors (loom.graph/successors g node)]
            (filter #(and (susceptible? g %)
                          (< (rand) infection-prob))
                    (loom.graph/successors g node))))
        new-infected (distinct (mapcat find-infected non-quarantined))
        g (p :infect-add-new-infected-state-step
             (loom.attr/add-attr-to-nodes g :state :E new-infected))]
    ;; add to-I for every newly infected node.
    (p :infect-add-to-Is
       (reduce (fn [g node]
                 (loom.attr/add-attr g node :to-I (to-I step)))
               g
               new-infected))))

(defnp infection-step
  "infection_step (S -> E):
  Each non-Quarantined Infected or Exposed node (in the final 2
  days before becoming Infected) is infectious.
  Each infectious nodes infects each of their non-Quarantined
  Susceptible neighbors with a certain probability.
  Nodes who were infected become Exposed and start their
  incubation time, which has a Gamma distribution."
  [g]
  (let [Is (p :nodes-by-state-I (nodes-by-state g :I))
        prob-infected-by-I 0.0044       ; 0.022 per day, 5 steps per day
        EIs (p :nodes-by-state-EI (nodes-by-state g :EI))
        prob-infected-by-EI (/ prob-infected-by-I 2)
        ;; to profile: uncomment g1, g2 and replace the return value with g2
        ;; g1 (p :infect-Is (infect g Is prob-infected-by-I))
        ;; g2 (p :infect-EIs (infect g1 EIs prob-infected-by-EI))
        ]
    (-> g
        (infect Is prob-infected-by-I)
        (infect EIs prob-infected-by-EI))))

(defnp incubation-step
  "incubation_step (E -> EI -> I):
  Exposed nodes whose incubation time has ended become Infected.
  Exposed nodes who have less than 2 days left to become Infected
  become infectious (:EI), but to a lesser degree."
  [g]
  (let [Es (nodes-and-attrs-by-state g :E)
        EIs (nodes-and-attrs-by-state g :EI)
        step (:step g)
        two-days (* 5 2)
        Es->EIs (map first
                     (filter
                      (fn [[_ {:keys [to-I]}]] (<= (- to-I two-days) step))
                      Es))
        EIs->Is (map first
                     (filter
                      (fn [[_ {:keys [to-I]}]] (<= to-I step))
                      EIs))]
    (-> (loom.attr/add-attr-to-nodes g :state :EI Es->EIs)
        (loom.attr/add-attr-to-nodes :state :I EIs->Is))))

(defn detect-infected
  {:pre (:test-symptomatic? g)}
  [g]
  (let [{:keys [mean-days-to-detect-infected]} g
        can-be-tested (fn [node] (nil? (loom.attr/attr g node :TP)))
        testable-infected (filter can-be-tested (nodes-by-state g :I))
        detection-prob (/ 1  (* 5 mean-days-to-detect-infected)) ; per simulation step
        ]
    (filter #(< (rand) detection-prob) testable-infected)))

(defn detect-from-population
  {:pre (:test-everyone? g)}
  [g]
  (let [{:keys [tests-per-1m-people n-nodes]} g
        can-be-tested (fn [node] (and (or (susceptible? g node)
                                          (exposed? g node))
                                      (nil? (loom.attr/attr g node :TP))))
        testable-nodes (filterv can-be-tested (nodes g))
        n-tests-per-step (Math/round
                          (/ tests-per-1m-people (/ 1000000 n-nodes) 5))
        n-tests-per-step (min n-tests-per-step (count testable-nodes))

        sampled-nodes (if (>= (* 10 n-tests-per-step) (count testable-nodes))
                        (take n-tests-per-step (shuffle testable-nodes)))]
    (filter #(or (infected? g %) (EI? g %) (exposed? g %))
            (take n-tests-per-step (distinct (repeatedly #(rand-nth testable-nodes)))))))

(defnp testing-step
  [g]
  (let [detected-infected (when (:test-symptomatic? g)
                            (detect-infected g))
        ;; TODO: general population testing
        detected-from-population (when (:test-everyone? g)
                                   (detect-from-population g))]
    (concat detected-infected detected-from-population)))

(defnp testing-and-quarantine-step
  ;; TODO: to model quarantine of non TP nodes ("contact tracing") -
  ;; add :Q attr to nodes
  "testing_step (update TP):
   A test is positive if a node is either Exposed or Infectious.
   Tests of symptomatic: some proportion of Infected nodes are tested
     (and test positive).
   Mass-testing: some proportion of the entire population is tested.
   Contact tracing: some proportion of neighbors of those who tested
     positive, are also tested.

   quarantine_step (update Q):
   Nodes who tested positive enter Quarantine for 14 days.
     Optionally, all neighbors of nodes who tested positive also
     enter Quarantine for 14 days.
   Quarantined nodes who finished their quarantine time exit
     quarantine (and retain their SEIR state).
   Quarantined nodes who tested positive in the past and are now
     Recovered exit Quarantine, even if their 14 days aren’t up.
   Quarantined nodes who tested positive in the past and haven't
     recovered stay in Quarantine, even if their 14 days are up.
   Quarantined individuals can neither infect nor be infected."
  [g]
  (let [step (:step g)
        new-TPs (testing-step g)
        Qs (filter (fn [[node attrs]] (:TP attrs)) (:attrs g))
        ;; S-and-Q-time-ended (map first
        ;;                         (filter
        ;;                          (fn [node attrs]
        ;;                            (and
        ;;                             (>= step (:TP attrs))
        ;;                             (= :S (:state attrs))))
        ;;                          Qs))
        Q-recovered (map first
                         (filter
                          (fn [[node attrs]] (= :R (:state attrs)))
                          Qs))]
    (-> g
        (loom.attr/add-attr-to-nodes :TP step new-TPs)
        (loom.attr/add-attr-to-nodes :TP nil Q-recovered))))

(defnp recovery-step
  "recovery_step (I -> R):
  Some proportion of Infected nodes become Recovered, and are no
  longer infectious. This has no memory, so it creates an
  exponentially distributed recovery time."
  [g]
  (let [infected (nodes-by-state g :I)
        ;; 1 / 3.5 to recover every day, 1 / (3.5 * 5) per step
        recovered (filter (fn [_] (< (rand) (/ 1 (* 3.5 5))))
                          infected)]
    (loom.attr/add-attr-to-nodes g :state :R recovered)))

(defnp initialize
  [{:keys [n-nodes mean-degree gamma] :as params}]
  (->
   (apply loom.graph/graph (range n-nodes))
   (loom.attr/add-attr-to-all :state :S)    ; initially: all are susceptible
   (connect mean-degree gamma)
   (random-infect 100)
   (random-expose 145)
   (merge params)
   (assoc :step 0)))

(defnp simulation-step
  [g]
  (-> g
      infection-step
      incubation-step
      testing-and-quarantine-step
      recovery-step
      (update :step inc)))

(defnp graph->counts
  [g]
  (let [attrs (vals (:attrs g))
        freqs (frequencies (map :state attrs))
        {:keys [S E EI I R] :or {S 0 E 0 EI 0 I 0 R 0}} freqs
        E (+ E EI)                      ; EI is implementation-internal
        Q (count (filter :TP attrs))]
    [{:state :S :count S :order 4}
     {:state :E :count E :order 3}
     {:state :I :count I :order 2}
     {:state :R :count R :order 1}
     {:state :Q :count Q :order 0}]))

(comment
  (tufte/add-basic-println-handler! {})
  (defnp benchmark-simulation
    []
    (loop [g (initialize {:n-nodes 5000})
           step 0]
      (when (>= 300 step)
        (when (zero? (mod step 10)) (prn "recuring" step))
        (recur (simulation-step g) (inc step))))))
