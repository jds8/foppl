(require '[clojure.data.json :as json])
(require '[clojure.core.match :refer [match]])
(require '[anglican.runtime :refer :all])
(load "primitives")
(require '[clojure.java.shell :as shell :refer [sh]])
(use '[clojure.java.shell :only [sh]])

(def all-records (json/read-str (slurp "/Users/MacMag/Desktop/Computer Science/ProbProg/CS532-HW2/1.json")
                :key-fn str))

(defn desugar [i]
  (json/read-str ((shell/with-sh-dir
    "/Users/MacMag/Desktop/Computer Science/ProbProg/daphne"
    (sh "lein" "run" "-f" "json" "desugar" "-i" (str "../CS532-HW2/programs/" i ".daphne"))) :out)))

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
         [c] (let [cv (get l c false)] (if (not (= cv false)) [cv s]) [(double c) s])
         ))

(def rho {})

(defn evaluate_program [ast]
  (match [ast]
         [[["defn" nm vs body] & t]] (do (def rho (merge rho {nm [vs body]})) (evaluate_program t))
         [[h]] (evaluate h [] {})))

(evaluate_program all-records)
