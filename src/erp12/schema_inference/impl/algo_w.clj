(ns erp12.schema-inference.impl.algo_w
  (:require [clojure.datafy :refer [datafy]]
            [erp12.schema-inference.impl.util :as u]
            [malli.core :as m]
            [malli.provider :as mp]))

(defmulti algo-w (fn [{:keys [op]} & _] op))

(defn- algo-w-failure?
  [x]
  (and (map? x) (some? (::failure x))))

(defn infer-schema
  [ast env]
  (let [{::keys [schema] :as result} (algo-w ast env)]
    (if (algo-w-failure? result)
      (throw (ex-info "Schema inference failure." result))
      schema)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lambda Calc

;; @todo Propagate ::throw
;; @todo Provide more context info about failures.

(defmethod algo-w :LIT
  [{:keys [type val]} _]
  {::subs   {}
   ::schema (case type
              :class {:type Class}
              (m/ast (mp/provide [val])))})

(defmethod algo-w :VAR
  [{:keys [sym]} env]
  (let [sym (symbol sym)]
    (if (contains? env sym)
      {::subs   {}
       ::schema (u/instantiate (get env sym))}
      {::failure {:var-not-found sym}})))

(defmethod algo-w :APP
  [{:keys [fn args]} env]
  (let [s-var {:type :s-var :sym (gensym "s-")}
        {f-subs ::subs f-schema ::schema :as fn-result} (algo-w fn env)]
    (if (algo-w-failure? fn-result)
      fn-result
      (let [args-ti (loop [remaining-args args
                           env' (u/substitute-env f-subs env)
                           args-ti []]
                      (if (empty? remaining-args)
                        args-ti
                        (let [arg (first remaining-args)
                              {a-subs ::subs :as arg-ti} (algo-w arg env')]
                          (if (algo-w-failure? arg-ti)
                            arg-ti
                            (recur (rest remaining-args)
                                   (u/substitute-env a-subs env')
                                   (conj args-ti arg-ti))))))]
        (if (algo-w-failure? args-ti)
          args-ti
          (let [subs (->> args-ti
                          (map ::subs)
                          reverse
                          (reduce u/compose-substitutions {}))
                subs' (u/mgu (u/substitute subs f-schema)
                             {:type   :=>
                              :input  {:type     :cat
                                       :children (mapv ::schema args-ti)}
                              :output s-var})]
            (if (u/mgu-failure? subs')
              {::failure {:unification-failure subs'}}
              {::subs   (u/compose-substitutions subs' subs)
               ::schema (u/substitute subs' s-var)})))))))

(defmethod algo-w :ABS
  [{:keys [params body] :as ast} env]
  ;; @todo Support variadic functions.
  (when (some :variadic? params)
    (throw (ex-info "Variadic functions not supported." {:ast ast})))
  (let [param-names (map :name params)
        s-vars (vec (repeatedly (count params) #(hash-map :type :s-var :sym (gensym "s-"))))
        env' (into env (map vector param-names s-vars))
        {::keys [subs schema] :as result} (algo-w body env')]
    (if (algo-w-failure? result)
      result
      {::subs   subs
       ::schema {:type   :=>
                 :input  {:type     :cat
                          :children (mapv #(u/substitute subs %) s-vars)}
                 :output schema}})))

(defmethod algo-w :LET
  [{:keys [bindings body]} env]
  (loop [remaining bindings
         env' env
         subs {}]
    (if (empty? remaining)
      (let [{body-subs ::subs body-schema ::schema :as result} (algo-w body (u/substitute-env subs env'))]
        (if (algo-w-failure? result)
          result
          {::subs   (u/compose-substitutions body-subs subs)
           ::schema body-schema}))
      (let [{:keys [name init]} (first remaining)
            {local-subs ::subs local-schema ::schema :as result} (algo-w init env')]
        (if (algo-w-failure? result)
          result
          (let [env' (dissoc env' name)
                local-schema' (u/generalize (u/substitute-env local-subs env) local-schema)]
            (recur (rest remaining)
                   (assoc env' name local-schema')
                   (u/compose-substitutions local-subs subs))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure

(defmethod algo-w :binding [_ _] (assert false "Should be unreachable."))

;(defmethod algo-w :case [ast env])
;(defmethod algo-w :case-test [ast env])
;(defmethod algo-w :case-then [ast env])

(defmethod algo-w :catch
  [{:keys [body]} env]
  (algo-w body env))

(defmethod algo-w :const
  [ast env]
  (algo-w (assoc ast :op :LIT) env))

(defmethod algo-w :def
  [{:keys [name init]} env]
  {::subs   (if (nil? init)
              {}
              {name (infer-schema init env)})
   ::schema {:type 'var?}})

;(defmethod algo-w :deftype [ast env] {::subs })

(defmethod algo-w :do
  [{:keys [ret]} env]
  (algo-w ret env))

(defmethod algo-w :fn
  [{:keys [methods] :as ast} env]
  ;; @todo Support multiple methods (aka overloading).
  (if (= (count methods) 1)
    (algo-w (first methods) env)
    (throw (ex-info "Cannot infer schema of functions with multiple methods."
                    {:ast ast}))))

(defmethod algo-w :fn-method
  [ast env]
  (algo-w (assoc ast :op :ABS) env))

;; @todo Support classes as ground types!
;; @todo Consider reflection instead of the type environment for interop ASTs. (JVM only).
;(defmethod algo-w :host-interop
;  [{:keys [target m-or-f]} env])

(defmethod algo-w :if
  [{:keys [test then else]} env]
  (algo-w {:op   :APP
           :fn   {:op  :var
                  :var 'clojure.core/if}
           :args [test then else]}
          env))

(defmethod algo-w :import
  [_ _]
  ;; @todo Should this check the AST is a symbol constant?
  {::subs {} ::schema nil?})

;(defmethod algo-w :instance-call [ast env])
;(defmethod algo-w :instance-field [ast env])

(defmethod algo-w :instance?
  [_ _]
  ;; This AST node is used when a Class constant is in the AST, therefore we don't need to type check.
  ;; When a non-const AST is provided for the Class argument to instance?, an :invoke node will be used.
  {::subs {} ::schema 'boolean?})

(defmethod algo-w :invoke
  [ast env]
  (algo-w (assoc ast :op :APP) env))

;; @todo Implement record-like theory (HMaps, relational algebra, etc.)
;(defmethod algo-w :keyword-invoke [ast env])

(defmethod algo-w :let
  [ast env]
  (algo-w (assoc ast :op :LET) env))

(defmethod algo-w :letfn
  [ast env]
  ;; @todo This incorrectly fails to allow ahead-of-definition use of functions.
  (algo-w (assoc ast :op :LET) env))

(defmethod algo-w :local
  [{:keys [name]} env]
  (algo-w {:op :VAR :sym name} env))

;(defmethod algo-w :loop [ast env])
;(defmethod algo-w :map [{:keys [keys vals}} env])
;(defmethod algo-w :method [ast env])
;(defmethod algo-w :new
;  [{:keys [class args]} env]
;  (let [fn-var (symbol (.getName String) "<init>")]
;    (algo-w {:op :APP
;             :fn {:op :var :var fn-var}
;             :args args}
;            (assoc env
;              fn-var {:type   :=>
;                      :input (-> class datafy :members
;                                 (get (symbol (.getName class)))
;                                 first :parameter-types
;                                 (map (fn [sym] (c/cls-name->schema (c/sym->cls sym)))))
;                      :output (c/cls-name->schema class)}))))

(defmethod algo-w :prim-invoke
  [ast env]
  (algo-w (assoc ast :op :APP) env))

(defmethod algo-w :protocol-invoke
  [{:keys [target protocol-fn args]} env]
  (let [protocol (-> protocol-fn :meta :protocol deref :on-interface)
        {fn-schema ::schema fn-subs ::subs :as result} (algo-w protocol-fn env)]
    (if (algo-w-failure? result)
      result
      (let [env' (u/substitute-env fn-subs env)
            {target-schema ::schema target-subs ::subs :as result} (algo-w target env')]
        (cond
          (algo-w-failure? result)
          result

          ;; Use "currying-ish" strategy to check the rest of the args except the instance
          ;; of the protocol.
          (u/sub-schema? target-schema {:type protocol})
          (let [tmp-f (gensym "f-")]
            (algo-w {:op   :APP
                     :fn   {:op :local :name tmp-f}
                     :args args}
                    (assoc env'
                      tmp-f (u/substitute target-subs (update-in fn-schema [:input :children] rest)))))

          :else
          {::failure {:must-extend-protocol (.getName protocol)
                      :protocol-fn          protocol-fn
                      :got                  target-schema}})))))

(defmethod algo-w :quote
  [{:keys [expr]} env]
  (algo-w expr env))

;(defmethod algo-w :recur [ast env])
;(defmethod algo-w :reify [ast env])
;(defmethod algo-w :set [{:keys [items]} env])

(defmethod algo-w :set!
  [{:keys [val]} env]
  (algo-w val env))

(defmethod algo-w :static-call
  [{:keys [class method] :as ast} env]
  (algo-w (assoc ast
            :op :APP
            :fn {:op  :var
                 :var (symbol (.getName class) (name method))})
          env))

(defmethod algo-w :static-field
  [{:keys [class field]} _]
  {::subs   {}
   ::schema {:type (-> (datafy class)
                       :members
                       (get field)
                       first
                       :type
                       name
                       Class/forName)}})

(defmethod algo-w :the-var
  [_ _]
  {::subs   {}
   ::schema {:type 'var?}})

(defmethod algo-w :throw
  [_ _]
  ;; @todo Should type check the `exception` child AST, and confirm subtype of Throwable.
  {::subs {} ::schema ::throw})

(defmethod algo-w :try
  [{:keys [body]} env]
  ;; @todo Type check the catch clauses.
  (algo-w body env))

(defmethod algo-w :var
  [{:keys [var]} env]
  (algo-w {:op :VAR :sym var} env))

;(defmethod algo-w :vector [{:keys [items]} env])

(defmethod algo-w :with-meta
  [{:keys [expr]} env]
  (algo-w expr env))