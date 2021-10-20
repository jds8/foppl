(require '[clojure.data.json :as json])
(require '[clojure.core.match :refer [match]])
(require '[anglican.runtime :refer :all])
(load "primitives")
(require '[clojure.java.shell :as shell :refer [sh]])
(use '[clojure.java.shell :only [sh]])

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

(take 1 (get-stream all-records))

(defn diff [ret truth]
  (if (= (type truth) clojure.lang.PersistentVector)
    (reduce + (map - ret truth))
    (- ret truth)))

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

(defn cwd [] (let [pwd ((sh "pwd") :out)] (str (.substring pwd 0 (- (count pwd) 1)) "/")))
(defn desugar-tests [type i]
  (json/read-str ((shell/with-sh-dir
    (str (cwd) "../daphne")
    (sh "lein" "run" "-f" "json" "desugar" "-i" (str "../CS532-HW2/programs/tests/" type "/test_" i ".daphne"))) :out)))

(defn load-truth [type i]
  (load-file (str (cwd) "../CS532-HW2/programs/tests/" type "/test_" i ".truth")))

;; (defn run_prob_test [samples truth]
;;   (let [distrs {"normal" norm "beta" beta "exponential" expon "normalmix" normalmix}]
;;     ))

;; (defn run-probabilistic-tests []
;;   (let [num_samples (int 1e4) max_p_value 1e-4]
;;     (for [i (range 1 7)]
;;       (do (print (str "running test " i " "))
;;       (let [ast (desugar-tests "probabilistic" i)
;;             truth (load-truth "probabilistic" i)
;;             samples (take num_samples (get-stream ast))
;;             p_val (run_prob_test samples truth)]
;;               (if (> (p_val max_p_value))
;;                 (do (println "failed test " i) (str "failed test " i))
;;                 (do (println "passed test " i) (str "passed test " i))))))))


(def all-records (desugar-tests "deterministic" 13))
(evaluate-program all-records)

(run-deterministic-tests)

(defn desugar-programs [i]
  (json/read-str ((shell/with-sh-dir
    (str (cwd) "../daphne")
    (sh "lein" "run" "-f" "json" "desugar" "-i" (str "../CS532-HW2/programs/" i ".daphne"))) :out)))
