(require '[clojure.data.json :as json])
(require '[clojure.core.match :refer [match]])
(load "primitives")
(require '[clojure.java.shell :as shell :refer [sh]])
(use '[clojure.java.shell :only [sh]])
(use '(incanter core stats charts io))

(def rho {})
(defn evaluate [e s l]
  (match [e]
         [["sample" d]] (let [[dist sig] (evaluate d s l)
                               smpl-fun (foppl.primitives/env "sample*")]
                                   [(smpl-fun dist) sig])
         [["observe" d v]] nil
         [["let" [v1 e1] e0]] (let [[c1 sig] (evaluate e1 s l)]
                                (evaluate e0 sig (assoc l v1 c1)))
         [["if" e1 e2 e3]] (let [[e1' sig] (evaluate e1 s l)]
                                (if e1' (evaluate e2 sig l) (evaluate e3 sig l)))
         ; TODO cs isn't strictly correct since I am getting rid of the sigmas
         ; unrelated to the above, we check if f is in primitives: if so, call it
         ; using a primitive, if not, assume it was a user-defined procedure
         [([e0 & t] :seq)] (let [cs (map (fn [exp] (first (evaluate exp s l))) t)
                                 f (get foppl.primitives/env e0 false)]
                             (if f [(apply f cs) s] (let [[vs e0'] (rho e0)]
                                 (evaluate e0' s (merge l (zipmap vs cs))))))
         [nil] nil
         ; TODO if we add strings as primitives, then this will have to change
         ; checking if cv is not false is weird, but (get l c false) returns nil
         ; for variables bound to the output of observe statements, so I need it
         [c] (let [cv (get l c false)] (if (not (= cv false)) [cv s] [c s]))
         ))

(defn evaluate-program [ast]
  (match [ast]
         [[["defn" nm vs body] & t]] (do (def rho (merge rho {nm [vs body]})) (evaluate-program t))
         [[h]] (evaluate h [] {})))

(defn get-stream
  ([ast] (get-stream ast 1))
  ([ast n] (lazy-seq (cons (evaluate-program ast) (get-stream ast (inc n))))))

(defn diff [ret truth]
  (if (= (type truth) clojure.lang.PersistentVector)
    (reduce + (map - ret truth))
    (- ret truth)))

(defn cwd [] (let [pwd ((sh "pwd") :out)] (str (.substring pwd 0 (- (count pwd) 1)) "/")))
(defn desugar-tests [type i]
  (json/read-str ((shell/with-sh-dir
    (str (cwd) "../daphne")
    (sh "lein" "run" "-f" "json" "desugar" "-i" (str "../CS532-HW2/programs/tests/" type "/test_" i ".daphne"))) :out)))

(defn load-truth [type i]
  (load-file (str (cwd) "../CS532-HW2/programs/tests/" type "/test_" i ".truth")))

(defn run-deterministic-tests []
  (for [i (range 1 13)]
    (do (print (str "running test " i " "))
    (let [ast (desugar-tests "deterministic" i)
          truth (load-truth "deterministic" i)
          [ret sig] (evaluate-program ast)]
            (if (> (abs (diff truth ret)) 0.001)
              (let [msg (str "return value " ret " is not equal to truth " truth " for exp " ast)]
                    (do (println msg) msg))
                    (do (println "passed test " i) (str "passed test " i)))))))

;; (defn run-prob-test [samples]
;;   (let [
;;         sorted (sort samples)
;;         idx (range 1 (+ (count samples) 1))
;;         fracs (map (fn [x] (/ x (count samples))) idx)
;;         D+ (apply max (map - fracs sorted))
;;         D- (apply max (map - sorted (map (fn [x] (- x (/ 1 (count samples)))) fracs)))
;;         D (max D+ D-)
;;         ]
;;     (> D 0.565)
;;     ))

;; (defn run-probabilistic-tests []
;;   (let [num-samples (int 1e4) max_p_value 1e-4]
;;     (for [i (range 1 7)]
;;       (do (print (str "running test " i " "))
;;       (let [ast (desugar-tests "probabilistic" i)
;;             samples (map (fn [x] (first x)) (take num-samples (get-stream ast)))
;;             passed (run-prob-test samples)]
;;               (if passed
;;                 (do (println "failed test " i) (str "failed test " i))
;;                 (do (println "passed test " i) (str "passed test " i))))))))

;; (run-probabilistic-tests)

(run-deterministic-tests)

(defn desugar-programs [i]
  (json/read-str ((shell/with-sh-dir
    (str (cwd) "../daphne")
    (sh "lein" "run" "-f" "json" "desugar" "-i" (str "../CS532-HW2/programs/" i ".daphne"))) :out)))

(defn sample-task [i num-samples]
  (let [ast (desugar-programs i)
        samples (map first (take num-samples (get-stream ast)))]
    samples))

(defn divide [x d]
  (match [x]
         [[h]] (vector (div h d))
         [([h & t] :seq)] (cons (divide h d) (map (fn [x] (divide x d)) t))
         [a] (/ a d)))

(defn calc-expectation [samples]
  (if (= (type (first samples)) clojure.lang.PersistentVector)
    (map (fn [x] (divide x (count samples))) (apply foppl.primitives/mat-add samples))
    (divide (reduce + samples) (count samples))))

;; calculate expectations for tasks 1-3
(def mu (sample-task 1 1000))
(def slope-bias (sample-task 2 1000))
(def hmm (sample-task 3 1000))
(def nn (sample-task 4 1000))

;; create conditional histograms
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

;; calculate expectations of first three tasks
(calc-expectation mu)
(calc-expectation slope-bias)
(calc-expectation hmm)

;; calculate expectation of neural network task
(def first-sum (apply foppl.primitives/mat-add (map (fn [x] (first x)) nn)))
(def second-sum (apply foppl.primitives/mat-add (map (fn [x] (first (rest x))) nn)))
(def third-sum (apply foppl.primitives/mat-add (map (fn [x] (first (rest (rest x)))) nn)))
(def fourth-sum  (apply foppl.primitives/mat-add (map (fn [x] (first (rest (rest (rest x))))) nn)))
(def full-mat (vector first-sum second-sum third-sum fourth-sum))
(def expected-full-mat (div full-mat (count nn)))
