(ns erp12.schema-inference.schema
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as w]
            [clojure.math.combinatorics :refer [combinations]]
            [erp12.schema-inference.ast :as ast]
            [malli.core :as m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates

(defn- is-a?
  [schema-kind schema]
  (m/validate schema-kind schema {:registry ast/registry}))

(defn atomic?
  [schema]
  (is-a? ::ast/Atomic schema))

(defn s-var?
  [schema]
  (is-a? ::ast/SVar schema))

(defn scheme?
  [scheme]
  (is-a? ::ast/Scheme scheme))

(defn fn-schema?
  [schema]
  (is-a? ::ast/Fn (get schema :body schema)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unpack Schemas

(defn fn-arg-schemas
  "Returns the schemas of a function schema's arguments."
  [schema]
  (rest (second (get schema :body schema))))

(defn fn-ret-schemas
  "Returns the return schema of a function schema."
  [schema]
  (last (get schema :body schema)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fresh Vars

(def gen-s-var-prefix "S_")

(defn gen-s-var
  "Generate a unique schema-var (s-var) symbol."
  []
  (gensym gen-s-var-prefix))

(defn gen-s-var?
  [v]
  (str/starts-with? (name v) gen-s-var-prefix))

(defn rename-s-vars
  "Rename the s-vars of the given scheme with fresh s-vars."
  [{:keys [s-vars with body] :or {with []}}]
  (let [renames (->> s-vars
                     (map #(vector % (gen-s-var)))
                     (into {}))
        smap (->> renames
                  (map (fn [[k v]] [[:s-var k] [:s-var v]]))
                  (into {}))
        renamed-constraints (w/postwalk-replace smap with)
        renamed-body (w/postwalk-replace smap body)]
    {:s-vars (vec (vals renames))
     :with   renamed-constraints
     :body   renamed-body}))

(defn free-type-vars
  "The free s-vars in the given schema, or collection of schema assumptions.
  For schema assumptions, returns the union of all free s-vars across all schemas."
  [schema-or-assumptions]
  (match schema-or-assumptions

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Types

    ;; Atomic Type
    (_ :guard ast/atomic?)
    #{}

    ;; Type var
    [:s-var t]
    #{t}

    ;; Function Type
    [:=> [:cat & arg-types] ret-type]
    (apply set/union (free-type-vars ret-type) (map free-type-vars arg-types))

    ;; Type Constructor
    [(ctor :guard keyword?) & type-args]
    (->> type-args
         (map free-type-vars)
         (reduce set/union))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Scheme

    {:s-vars [& type-vars] :body body}
    (set/difference (free-type-vars body) (set type-vars))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Environment

    (env :guard ast/env?)
    (reduce (fn [acc assumption]
              (match assumption
                [:= _ t] (set/union acc (free-type-vars t))
                :else acc))
            #{}
            env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitutions

(defn substitute-types
  "Uses the map of s-var substitutions to replace schemas in `x`.

  If `x` is a schema, substitution is straightforward.
  If `x` is a scheme, its s-vars are renamed to avoid causing collisions.
  If `x` is a collection of assumptions, the substitutions are applied to every assumption.

  The `subs` map should have symbols for keys and schema (or schemes) for values."
  [subs x]
  (match x

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Types

    ;; Ground Type
    (t :guard ast/atomic?)
    x

    ;; Function type
    [:=> [:cat & arg-types] ret-type]
    [:=> (vec (cons :cat (map #(substitute-types subs %) arg-types))) (substitute-types subs ret-type)]

    ;; Type Variable
    [:s-var t]
    (get subs t [:s-var t])

    ;; Type Constructor
    [(ctor :guard keyword?) & type-args]
    (vec (cons ctor (map #(substitute-types subs %) type-args)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Scheme

    {:s-vars [& type-vars] :body body}
    (let [renamed (rename-s-vars x)]
      (assoc renamed :body (substitute-types subs (:body renamed))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Environment

    (env :guard ast/env?)
    (mapv (fn [assumption]
            (match assumption
              [:= v t]
              [:= v (substitute-types subs t)]

              [:< sub-type super-type]
              [:< (substitute-types subs sub-type) (substitute-types subs super-type)]))
          env)))

(defn compose-substitutions
  "Combines 2 sets of type substitutions."
  [subs1 subs2]
  (merge (into {} (map (fn [[a t]] [a (substitute-types subs1 t)])
                       subs2))
         (into {} (apply dissoc subs1 (keys subs2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assumptions

(def empty-assumptions [])

(defn typings
  "Filters the assumptions to only typing assumptions.
  ie. `[:= 'usernames [:vector :string]]`"
  [assumptions]
  (filter #(= := (first %)) assumptions))

(defn typings-of
  "Returns a collection of typings of the `symb` variable.
  If the result has multiple elements, `symb` is overloaded."
  [assumptions symb]
  (mapcat (fn [a]
            (match a
              [:= (s :guard #(= symb %)) t] [t]
              :else nil))
          assumptions))

(defn sub-typings
  "Filters the assumptions to only sub-typing assumptions.
  ie. `[:< :int :number]`"
  [assumptions]
  (filter #(= :< (first %)) assumptions))

(defn super-types-of
  "Returns a collection of super-types (as schemas) of the given `schema`."
  [assumptions schema]
  (mapcat (fn [a]
            (match a
              [:< (s :guard #(= schema %)) super] [super]
              :else nil))
          assumptions))

(defn sub-types-of
  "Returns a collection of sub-types (as schemas) of the given `schema`."
  [assumptions schema]
  (mapcat (fn [a]
            (match a
              [:< sub (s :guard #(= schema %))] [sub]
              :else nil))
          assumptions))

(defn constraints
  "Returns a vector of constraints declared by the given schema.
  Any schema that is not a scheme will have no constraints."
  [schema]
  (match schema
    {:s-vars _ :body _} (get schema :with [])
    :else []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Inference Helpers

(defn generalize
  "Creates a scheme by lifting all free type-vars into schema parameters."
  [env scheme]
  (let [s-vars (vec (set/difference (free-type-vars scheme) (free-type-vars env)))]
    (if (empty? s-vars)
      scheme
      {:s-vars s-vars :body scheme})))

(defn instantiate
  "Creates a schema from a schema-scheme by replacing all type parameters with fresh type vars."
  [type-or-scheme]
  (match type-or-scheme

    {:s-vars [& s-vars] :body body}
    (let [subs (into {} (map #(vector %1 [:s-var %2])
                             s-vars
                             (repeatedly gen-s-var)))]
      (substitute-types subs body))

    [:s-var t]
    [:s-var t]

    :else type-or-scheme))

(defn concretize
  "Creates a concrete schema from a scheme and the schemas to bind to the scheme args."
  [scheme s-var->schema]
  (->> scheme :body (substitute-types s-var->schema) (generalize [])))

(defn bind-type-var
  [s-var t]
  (cond
    (= [:s-var s-var] t)
    {}

    (contains? (free-type-vars t) s-var)
    (throw (ex-info (format "Can't bind %s to %s because occurs check fails." t s-var)
                    {:s-var s-var
                     :type  t}))

    :else
    {s-var t}))

(defn same-ctor?
  [sch1 sch2]
  (if (and (vector? sch1) (vector? sch2))
    (= (first sch1) (first sch2))
    (= sch1 sch2)))

(defn mgu
  "Find the most general unifier for both schemas."
  [s1 s2]
  (match [s1 s2]

    [[:s-var s] t]
    (bind-type-var s t)

    [t [:s-var s]]
    (bind-type-var s t)

    [[:=> [:cat & arg-types1] ret-type1]
     [:=> [:cat & arg-types2] ret-type2]]
    (if (not= (count arg-types1) (count arg-types2))
      (throw (ex-info (format "Types %s and %s do not unify." s1 s2)
                      {:unification-failure :mismatched-function-arity
                       :schema-1            s1
                       :schema-2            s2}))
      (let [subs1 (->> (map vector arg-types1 arg-types2)
                       (reduce (fn [subs [arg1 arg2]]
                                 (->> (mgu (substitute-types subs arg1)
                                           (substitute-types subs arg2))
                                      (compose-substitutions subs)))
                               {}))
            subs2 (mgu (substitute-types subs1 ret-type1)
                       (substitute-types subs1 ret-type2))]
        (compose-substitutions subs1 subs2)))

    [[ctor1 & args1] [ctor2 & args2]]
    (cond
      (not= ctor1 ctor2)
      (throw (ex-info (format "Types %s and %s do not unify." s1 s2)
                      {:unification-failure :mismatched-schema-ctor
                       :schema-1            s1
                       :schema-2            s2}))

      (not= (count args1) (count args2))
      (throw (ex-info (format "Types %s and %s do not unify." s1 s2)
                      {:unification-failure :mismatched-ctor-args
                       :schema-1            s1
                       :schema-2            s2}))

      :else
      (let [all-subs (try (map mgu args1 args2)
                          (catch clojure.lang.ExceptionInfo e
                            (throw (ex-info (format "Types %s and %s do not unify." s1 s2)
                                            {:unification-failure :nested-unification
                                             :schema-1            s1
                                             :schema-2            s2
                                             :nested-data         (ex-data e)}
                                            e))))]
        (reduce compose-substitutions all-subs)))

    [a b]
    (if (= a b)
      {}
      (throw (ex-info (format "Types %s and %s do not unify." a b)
                      {:unification-failure :standard
                       :schema-1            a
                       :schema-2            b})))))

(defn safe-mgu
  "Same as `mgu` but will return nil instead of throwing when unification fails."
  [s1 s2]
  (try (mgu s1 s2)
       (catch clojure.lang.ExceptionInfo e
         (if (contains? (ex-data e) :unification-failure)
           nil
           (throw e)))))

(defn unifiable?
  "Return `true` is the schemas are unifiable, otherwise false."
  [s1 s2]
  (boolean (safe-mgu s1 s2)))

(defn lcg
  "Compute the Least Common Generalization for a set of schemas.

  See: Foundations of Inductive Logic Programming - Carbonell & Siekmann
  See: http://www.cs.cmu.edu/afs/cs/user/jcr/ftp/transysalg.pdf"
  ([s1 s2]
   (loop [a (if (ast/scheme? s1) (:body s1) s1)
          b (if (ast/scheme? s2) (:body s2) s2)
          a-inverse-subs {}
          b-inverse-subs {}
          i 0]
     (if (= a b)
       (generalize empty-assumptions a)
       (let [p (ast/pos-of-first-diff a b)
             t1 (ast/term-at-position a p)
             t2 (ast/term-at-position b p)
             a-var (get a-inverse-subs t1)
             b-var (get b-inverse-subs t2)]
         (if (and (some? a-var) (= a-var b-var))
           (recur (assoc-in a p a-var)
                  (assoc-in b p b-var)
                  a-inverse-subs
                  b-inverse-subs
                  i)
           (let [z [:s-var (gen-s-var)]]
             (recur (if (= p []) z (assoc-in a p z))
                    (if (= p []) z (assoc-in b p z))
                    (assoc a-inverse-subs t1 z)
                    (assoc b-inverse-subs t2 z)
                    (inc i))))))))
  ([s1 s2 & args]
   (reduce lcg (lcg s1 s2) args)))

(defn coupled?
  "Return `true` if the schema, or collection of assumptions, is coupled. Otherwise, `false`."
  [schema-or-env]
  (match schema-or-env

    ;; Ground Type
    (t :guard ast/atomic?)
    true

    ;; Function type
    [:=> [:cat & _] _]
    true

    ;; Type Variable
    ; [:s-var _]
    ; @todo Not sure what the result should be here. Probably never will happen. Leaving as match error for now.

    ;; Type Constructor
    [(_ :guard keyword?) & _]
    true

    ;; Scheme
    {:s-vars [& type-vars] :body body}
    (every? #(ast/occurs? % body) type-vars)

    ;; Typing
    [:= v t]
    (coupled? t)

    ;; Subtyping @todo What to do here?
    [:< sub super]
    true

    ;; Environment
    (env :guard ast/env?)
    (every? coupled? env)))


(defn overlapping?
  "Return `true` if the two schemas, or collection of assumptions, is overlapping. Otherwise, `false`."
  ([assumptions]
   (boolean (some (fn [[a1 a2]] (overlapping? (last a1) (last a2)))
                  (combinations (typings assumptions) 2))))
  ([sch1 sch2]
   (let [safe-get-body #(if (ast/scheme? %) (:body (rename-s-vars %)) %)]
     (unifiable? (safe-get-body sch1) (safe-get-body sch2)))))

(defn satisfiable?
  "Return `true` if the assumptions can satisfiable given the environment. Otherwise, `false`."
  [env assumptions]
  {:pre [(coupled? env)
         (not (overlapping? env))
         ;; @todo Assumptions must only contain ground types.
         ;; @todo Env is an overloading "by constructors"
         ]}
  (loop [unsat assumptions]
    (if (empty? unsat)
      true
      (let [body #(if (ast/scheme? %) (:body %) %)
            [_ symb typ :as assumption] (first unsat)
            [subs satisfier] (some (fn [typing]
                                     (when-let [subs (safe-mgu (body typ) (body typing))]
                                       [subs typing]))
                                   (typings-of env symb))]
        (if (nil? satisfier)
          false
          (recur (->> unsat
                      (remove #(= assumption %))
                      (concat (substitute-types subs (constraints satisfier)))
                      vec)))))))

;; @todo Implement close
;(defn close
;  [env assumptions schema]
;  (let [env-ftv (free-type-vars env)
;        new-assumptions (if (empty? env-ftv)
;                          (if (satisfiable? env assumptions)
;                            {}
;                            (throw (ex-info "Failed to close schema."
;                                            {:schema      schema
;                                             :assumptions assumptions
;                                             :env         env})))
;                          assumptions)
;        s-vars (set/difference (set/union (free-type-vars assumptions)
;                                          (free-type-vars schema))
;                               env-ftv)
;        new-constraints (->> assumptions
;                             (filter (fn [rule]
;                                       (some #(ast/occurs? % rule)
;                                             s-vars)))
;                             vec)]
;    [new-assumptions
;     (instantiate {:s-vars (vec s-vars)
;                   :with   new-constraints
;                   :body   schema})]))
