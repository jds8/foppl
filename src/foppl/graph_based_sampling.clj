(require '[clojure.data.json :as json])
(require '[clojure.core.match :refer [match]])
(require '[anglican.runtime :refer :all])
(load "primitives")
(require '[clojure.java.shell :as shell :refer [sh]])
(use '[clojure.java.shell :only [sh]])

(def all-records (json/read-str (slurp (str (cwd) "../CS532-HW2/graph_test_2.json"))
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
         [d] (if (or (= (type d) java.lang.Long) (= (type d) java.lang.Double)) d
                 (if (= (type d) java.lang.String) (var-map d)
                    (throw (Exception. (str "Expression type unknown." d)))
         ))))

(defn call-prob-eval [var-map var graph]
  (let [link-fun (((second graph) "P") var)]
    (assoc var-map var (probabilistic-eval link-fun var-map))
  ))

(defn get-parents-keys [var parents akeys A]
  (if (empty? akeys) parents
    (let [key (first akeys) tail (rest akeys)]
      (if (some #(= var %) (A key)) (get-parents-keys var (foppl.primitives/append parents key) tail A) (get-parents-keys var parents tail A))
  )))

(defn get-parents-from-dict [var A]
  (get-parents-keys var '() (keys A) A))

(defn get-parents [var graph]
  (get-parents-from-dict var ((second graph) "A")))

(defn top-sort [queue in-q vars G]
  (if (empty? vars) [queue in-q]
    (let [var (first vars) tail (rest vars)]
      (if (get in-q var false) (top-sort queue in-q tail G)
        (let [parents (get-parents var G) new-queues (top-sort queue in-q parents G)]
          (top-sort (foppl.primitives/append (first new-queues) var)
                    (assoc (last new-queues) var true) tail G)))
)))

(defn top-sort-graph [G]
  (top-sort [] {} (keys ((second G) "P")) G))

(defn sample-from-joint [graph]
  (let [queues (top-sort-graph graph)
        vars (first queues)
        samples (reduce
                 (fn [new-map var] (call-prob-eval new-map var graph)) {} vars)
        return (probabilistic-eval (last graph) samples)]
    return
  ))

(defn diff [ret truth]
  (if (= (type truth) clojure.lang.PersistentVector)
    (reduce + (map - ret truth))
    (- ret truth)))

(defn cwd [] (let [pwd ((sh "pwd") :out)] (str (.substring pwd 0 (- (count pwd) 1)) "/")))
(defn graph-tests [type i]
  (json/read-str ((shell/with-sh-dir
    (str (cwd) "../daphne")
    (sh "lein" "run" "-f" "json" "desugar" "-i" (str "../CS532-HW2/programs/tests/" type "/test_" i ".daphne"))) :out)))

(defn run-deterministic-tests []
  (for [i (map (fn [x] (+ 1 x)) (range 13))]
    (do (println (str "running test " i))
    (let [ast (graph-tests "deterministic" i)
          truth (load-truth i)
          [ret sig] (evaluate-program ast)]
            (if (> (abs (diff truth ret)) 0.001)
              (let [msg (str "return value " ret " is not equal to truth " truth " for exp " ast)]
                    (do (print msg) msg)
              (do (print "passed test " i) (str "passed test " i))))))
  )

(run-deterministic-tests)
