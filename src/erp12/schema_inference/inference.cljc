(ns erp12.schema-inference.inference
  (:require [clojure.core.match :refer [match]]
            [malli.core :as m]
            [erp12.schema-inference.schema :as sch]
            [erp12.schema-inference.utils :as u]))

(def atomic-ground-schemas
  [nil? boolean? int? float? char? string? keyword?])

(declare infer-ground-schema)

;; @todo Wrap element schema in :maybe if collection contains nil. Requires support for sub-types.
(defn- infer-element-schema
  [coll]
  (infer-ground-schema (u/first-non-nil coll)))

(defn- infer-atomic-schema
  [x]
  (loop [remaining atomic-ground-schemas]
    (let [schema (first remaining)]
      (cond
        (empty? remaining) nil
        (m/validate schema x) schema
        :else (recur (rest remaining))))))

(defn infer-ground-schema
  [x]
  (cond
    (vector? x) [:vector (infer-element-schema x)]
    (set? x) [:set (infer-element-schema x)]
    (map? x) [:map-of
              (infer-element-schema (keys x))
              (infer-element-schema (vals x))]
    :else (infer-atomic-schema x)))

(defn algo-w
  "Performs Algorithm-W on the `expr` under the environment."
  [env expr]
  (match [expr]

    ;; Const
    [[:lit v]]
    [{} (infer-ground-schema v)]

    ;; Var
    [[:var v]]
    (let [sigma (first (sch/typings-of env v))]
      ;; @todo Update sigma for overloaded variables
      (when (nil? sigma)
        (throw (ex-info (format "Unbound variable: %s" v)
                        {:expr     expr
                         :variable v
                         :env      env})))
      [{} (sch/instantiate sigma)])

    ;; Abs
    [[:fn [:cat & args] & body]]
    (let [type-vars (repeatedly (count args) #(vector :s-var (sch/gen-s-var)))
          new-env (->> args
                       (map #(match [%] [[:var s]] s))
                       (map #(vector := %2 %1) type-vars)
                       (concat env)
                       vec)
          [subs body-type] (algo-w new-env (last body))]
      [subs
       (sch/substitute-types subs [:=> (vec (cons :cat type-vars)) body-type])])

    ;; App
    [[:apply f & args]]
    (let [t-var [:s-var (sch/gen-s-var)]
          [f-subs f-type] (algo-w env f)
          arg-ti (map #(algo-w (sch/substitute-types f-subs env) %) args)
          subs-u (->> arg-ti (map first) (reduce sch/compose-substitutions))
          subs (sch/mgu (sch/substitute-types subs-u f-type)
                        [:=> (vec (cons :cat (map second arg-ti))) t-var])]
      [(sch/compose-substitutions subs subs-u) (sch/substitute-types subs t-var)])

    ;; Let
    [[:let [& vars-&-defs] & body]]
    (loop [remaining (partition 2 vars-&-defs)
           in-env env
           subs {}]
      (if (empty? remaining)
        (let [[new-subs typ] (algo-w (sch/substitute-types subs in-env) (last body))]
          [(sch/compose-substitutions subs new-subs) typ])
        (let [[var var-def] (first remaining)
              [new-subs typ] (algo-w in-env var-def)
              new-env (->> env
                           (remove (fn [a]
                                     (match a
                                       [:= (s :guard #(= var %)) _] true
                                       :else false)))
                           (concat [[:= (second var) (sch/generalize (sch/substitute-types subs in-env) typ)]])
                           vec)]
          (recur (rest remaining)
                 new-env
                 new-subs))))))

(defn infer-schema
  "Infer the type of the expression given the environment."
  [env expr]
  (let [[subs t] (algo-w env expr)
        inferred (sch/substitute-types subs t)
        ftv (sch/free-type-vars inferred)]
    (if (empty? ftv)
      inferred
      ;; Set to vec may produce unstable ordering.
      {:s-vars (vec ftv)
       :with   []
       :body   inferred})))
