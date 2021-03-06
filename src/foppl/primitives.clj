(ns foppl.primitives
  (:require [anglican.runtime :refer :all])
  (:require [anglican.core :refer :all])
  (:require [clojure.core.matrix :as m])
  (:require [anglican.runtime :refer [tanh]]))

(def ^:dynamic *primitive-procedures*
  "primitive procedures, do not exist in CPS form"
  (let [;; higher-order procedures cannot be primitive
        exclude '#{loop
                   map reduce
                   filter keep keep-indexed remove
                   repeatedly
                   every? not-any? some
                   every-pred some-fn
                   comp juxt partial}
        ;; runtime namespaces
        runtime-namespaces '[clojure.core anglican.runtime foppl.primitives]]
    (set (keep (fn [[k v]]
                 (when (and (not (exclude k))
                            (fn? (var-get v)))
                   k))
               (mapcat ns-publics runtime-namespaces)))))

  (defn append [& args] (apply conj args))

  (defn mat-mul [& args] (apply m/mmul args))
  (defn mat-add [& args] (apply m/add args))
  (defn mat-transpose [& args] (apply m/transpose args))
  (defn mat-tanh [M] (m/emap tanh M))
  (defn mat-relu [M] (m/emap (fn [x] (if (> x 0) x 0)) M))
  (defn mat-repmat [M r c]
    (let [R (reduce (partial m/join-along 0) (repeat r M))]
      (reduce (partial m/join-along 1) (repeat c R))))

(def env (hash-map "normal" #'normal
                   "sqrt" #'sqrt
                   "uniform-continuous" #'uniform-continuous
                   "beta" #'beta
                   "bernoulli" #'bernoulli
                   "exponential" #'exponential
                   "discrete" #'discrete
                   "append" #'foppl.primitives/append
                   "+" #'+
                   "-" #'-
                   "*" #'*
                   "/" #'/
                   "exp" #'exp
                   "vector" #'vector
                   "last" #'last
                   "nth" #'nth
                   "hash-map" #'hash-map
                   "put" #'assoc
                   "get"  #'get
                   "last" #'last
                   "first" #'first
                   "second" #'second
                   "rest" #'rest
                   "cons" #'cons
                   "conj" #'conj
                   "mat-add" #'mat-add
                   "mat-mul" #'mat-mul
                   "mat-repmat" #'mat-repmat
                   "mat-tanh" #'mat-tanh
                   "mat-transpose" #'mat-transpose
                   "sample*" #'sample*
                   "observe*" (fn [& args] nil)
                   ))
