(require '[clojure.data.json :as json])
(require '[clojure.core.match :refer [match]])
(require '[anglican.runtime :refer :all])
(load "primitives")

(def all-records (json/read-str (slurp "/Users/MacMag/Desktop/Computer Science/ProbProg/CS532-HW2/graph_test_2.json")
                :key-fn str))

(defn deterministic-eval [exp]
  (match [exp]
         [([h & t] :seq)] (apply (foppl.primitives/env h) (map deterministic-eval t))
         [d] (double d)
         :else (throw (Exception. (str "Expression type unknown." exp)))
         ))

(defn probabilistic-eval [exp var-map]
  (match [exp]
         [([h & t] :seq)]
         (apply (foppl.primitives/env h) (map (fn [e] (probabilistic-eval e var-map)) t))
         [d] (if (or (= (type d) java.lang.Long) (= (type d) java.lang.Double)) (double d)
                 (if (= (type d) java.lang.String) (var-map d)
                    (throw (Exception. (str "Expression type unknown." d)))
         ))))

(defn call-prob-eval [var-map var graph]
  (let [link-fun (((second graph) "P") var)]
    (assoc var-map var (probabilistic-eval link-fun var-map))
  ))

(defn sample-from-joint [graph]
  (let [queues (top-sort-graph graph)
        vars (first queues)
        samples (reduce
                 (fn [new-map var] (call-prob-eval new-map var graph)) {} vars)
        return (probabilistic-eval (last graph) samples)]
    return
  ))

(sample-from-joint all-records)

(defn get-parents [var graph]
  (get-parents-from-dict var ((second graph) "A")))

(defn get-parents-from-dict [var A]
  (get-parents-keys var '() (keys A) A))

(defn get-parents-keys [var parents akeys A]
  (if (empty? akeys) parents
    (let [key (first akeys) tail (rest akeys)]
      (if (some #(= var %) (A key)) (get-parents-keys var (foppl.primitives/append parents key) tail A) (get-parents-keys var parents tail A))
  )))

(defn top-sort-graph [G]
  (top-sort [] {} (keys ((second G) "P")) G))

(defn top-sort [queue in-q vars G]
  (if (empty? vars) [queue in-q]
    (let [var (first vars) tail (rest vars)]
      (if (get in-q var false) (top-sort queue in-q tail G)
        (let [parents (get-parents var G) new-queues (top-sort queue in-q parents G)]
          (top-sort (foppl.primitives/append (first new-queues) var)
                    (assoc (last new-queues) var true) tail G)))
)))

(top-sort-graph all-records)
