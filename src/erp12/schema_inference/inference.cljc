(ns erp12.schema-inference.inference
  (:require [clojure.core.match :refer [match]]
            [erp12.schema-inference.utils :as u]
            [malli.core :as m]))

(def ground-schemas
  [nil? boolean? int? float? char? string? keyword?])

;; @todo How to handle collection literals?

(defn infer-ground-schema
  [x]
  (loop [remaining ground-schemas]
    (let [schema (first remaining)]
      (cond
        (empty? remaining) nil
        (m/validate schema x) schema
        :else (recur (rest remaining))))))

(defn algo-w
  "Performs Algorithm-W on the `expr` under the environment."
  [env expr]
  (match [expr]

    ;; Const
    [[:lit v]]
    [{} (infer-ground-schema v)]

    ;; Var
    [[:var v]]
    (let [sigma (first (u/typings-of env v))]
      ;; @todo Update sigma for overloaded variables
      (when (nil? sigma)
        (throw (ex-info (format "Unbound variable: %s" v)
                        {:expr     expr
                         :variable v
                         :env      env})))
      [{} (u/instantiate sigma)])

    ;; Abs
    [[:fn [:cat & args] & body]]
    (let [type-vars (repeatedly (count args) #(vector :s-var (u/gen-s-var)))
          new-env (->> args
                       (map #(match [%] [[:var s]] s))
                       (map #(vector := %2 %1) type-vars)
                       (concat env)
                       vec)
          [subs body-type] (algo-w new-env (last body))]
      [subs
       (u/substitute-types subs [:=> (vec (cons :cat type-vars)) body-type])])

    ;; App
    [[:apply f & args]]
    (let [t-var [:s-var (u/gen-s-var)]
          [f-subs f-type] (algo-w env f)
          arg-ti (map #(algo-w (u/substitute-types f-subs env) %) args)
          subs-u (->> arg-ti (map first) (reduce u/compose-substitutions))
          subs (u/mgu (u/substitute-types subs-u f-type)
                      [:=> (vec (cons :cat (map second arg-ti))) t-var])]
      [(u/compose-substitutions subs subs-u) (u/substitute-types subs t-var)])

    ;; Let
    [[:let [& vars-&-defs] & body]]
    (loop [remaining (partition 2 vars-&-defs)
           in-env env
           subs {}]
      (if (empty? remaining)
        (let [[new-subs typ] (algo-w (u/substitute-types subs in-env) (last body))]
          [(u/compose-substitutions subs new-subs) typ])
        (let [[var var-def] (first remaining)
              [new-subs typ] (algo-w in-env var-def)
              new-env (->> env
                           (remove (fn [a]
                                     (match a
                                       [:= (s :guard #(= var %)) _] true
                                       :else false)))
                           (concat [[:= (second var) (u/generalize (u/substitute-types subs in-env) typ)]])
                           vec)]
          (recur (rest remaining)
                 new-env
                 new-subs))))))

(defn infer-schema
  "Infer the type of the expression given the environment."
  [env expr]
  (let [[subs t] (algo-w env expr)
        inferred (u/substitute-types subs t)
        ftv (u/free-type-vars inferred)]
    (if (empty? ftv)
      inferred
      ;; Set to vec may produce unstable ordering.
      {:s-vars (vec ftv)
       :with   []
       :body   inferred})))
