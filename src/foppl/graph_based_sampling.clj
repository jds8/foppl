(require '[clojure.data.json :as json])
(require '[clojure.core.match :refer [match]])
(require '[anglican.runtime :refer :all])
(load "primitives")
(require '[clojure.java.shell :as shell :refer [sh]])
(use '[clojure.java.shell :only [sh]])
(use '(incanter core stats charts io))

(defn cwd [] (let [pwd ((sh "pwd") :out)] (str (.substring pwd 0 (- (count pwd) 1)) "/")))
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
    (sh "lein" "run" "-f" "json" "desugar" "-i" (str "../CS532-HW2/original_programs/tests/" type "/test_" i ".daphne"))) :out)))

(defn run-deterministic-tests []
  (for [i (map (fn [x] (+ 1 x)) (range 13))]
    (do (println (str "running test " i))
    (let [ast (graph-tests "deterministic" i)
          truth (load-truth i)
          [ret sig] (evaluate-program ast)]
            (if (> (abs (diff truth ret)) 0.001)
              (let [msg (str "return value " ret " is not equal to truth " truth " for exp " ast)]
                    (do (print msg) msg)
              (do (print "passed test " i) (str "passed test " i))))))))

;; (run-deterministic-tests)

(defn graph-programs [i]
  (json/read-str ((shell/with-sh-dir
    (str (cwd) "../daphne")
    (sh "lein" "run" "-f" "json" "graph" "-i" (str "../CS532-HW2/original_programs/" i ".daphne"))) :out)))

(defn get-stream
  ([ast] (get-stream ast 1))
  ([ast n] (lazy-seq (cons (sample-from-joint ast) (get-stream ast (inc n))))))

(defn sample-task [ast num-samples]
  (let [samples (take num-samples (get-stream ast))]
    samples))

(defn divide [x d]
  (match [x]
         [[h]] (vector (/ h d))
         [([h & t] :seq)] (cons (divide h d) (map (fn [x] (divide x d)) t))
         [a] (/ a d)))

(defn calc-expectation [samples]
  (if (= (type (first samples)) clojure.lang.PersistentVector)
    (map (fn [x] (divide x (count samples))) (apply foppl.primitives/mat-add samples))
    (divide (reduce + samples) (count samples))))

;; generate programs
(def program1 (graph-programs 1))
(def program2 (graph-programs 2))
(def program4 (graph-programs 4))
(def program5 (graph-programs 5))

;; take 1000 samples for each task
(def mu (sample-task program1 1000))
(def slope-bias (sample-task program2 1000))
(def hmm (sample-task program4 1000))
(def nn (sample-task program5 1000))

;; create histograms
(save (histogram mu) "mu.png")

(def slope (map (fn [x] (first x)) slope-bias))
(def bias (map (fn [x] (second x)) slope-bias))
(save (histogram slope) "slope.png")
(save (histogram bias) "bias.png")

(def hmm1 (map (fn [x] (first x)) hmm))
(save (histogram hmm1) "hmm1.png")
(def hmm6 (map (fn [x] (get x 6)) hmm))
(save (histogram hmm6) "hmm6.png")
(def hmm11 (map (fn [x] (get x 11)) hmm))
(save (histogram hmm11) "hmm11.png")
(def hmm16 (map (fn [x] (get x 16)) hmm))
(save (histogram hmm16) "hmm16.png")

(def nnw0 (map (fn [x] (first (first (first x)))) nn))
(save (histogram nnw0) "nnw0.png")
(def nnb0 (map (fn [x] (first (first (get x 1)))) nn))
(save (histogram nnb0) "nnb0.png")
(def nnw1 (map (fn [x] (first (first (get x 2)))) nn))
(save (histogram nnw1) "nnw1.png")
(def nnb1 (map (fn [x] (first (first (get x 3)))) nn))
(save (histogram nnb1) "nnb1.png")

;; calculate expectations
(calc-expectation mu)
(calc-expectation slope-bias)
(calc-expectation hmm)

;; calculate expectation of neural network task
(def first-sum (apply foppl.primitives/mat-add (map (fn [x] (first x)) nn)))
(def second-sum (apply foppl.primitives/mat-add (map (fn [x] (first (rest x))) nn)))
(def third-sum (apply foppl.primitives/mat-add (map (fn [x] (first (rest (rest x)))) nn)))
(def fourth-sum  (apply foppl.primitives/mat-add (map (fn [x] (first (rest (rest (rest x))))) nn)))
(def full-mat (vector first-sum second-sum third-sum fourth-sum))
(def expected-full-mat (divide full-mat (count nn)))
