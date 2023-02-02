(ns erp12.schema-inference.impl.util
  (:require [clojure.set :as set]
            [erp12.schema-inference.impl.ground :as g]))

(defn ground?
  [{:keys [type] :as schema}]
  (and (= (count schema) 1)
       (or (ident? type) (class? type))
       (not= type :s-var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti free-type-vars (fn [s] (if (ground? s) :ground (:type s))))

(defmethod free-type-vars :ground [_] #{})
(defn- free-type-vars-ctor1 [{:keys [child]}] (free-type-vars child))
(defmethod free-type-vars :vector [schema] (free-type-vars-ctor1 schema))
(defmethod free-type-vars :set [schema] (free-type-vars-ctor1 schema))
(defmethod free-type-vars :sequential [schema] (free-type-vars-ctor1 schema))
(defmethod free-type-vars :maybe [schema] (free-type-vars-ctor1 schema))

(defn- free-type-vars-ctorN
  [{:keys [children]}]
  (reduce #(set/union %1 (free-type-vars %2)) #{} children))

(defmethod free-type-vars :tuple [schema] (free-type-vars-ctorN schema))
(defmethod free-type-vars :cat [schema] (free-type-vars-ctorN schema))

(defmethod free-type-vars :map-of
  [{:keys [key value]}]
  (set/union (free-type-vars key) (free-type-vars value)))

(defmethod free-type-vars :=>
  [{:keys [input output]}]
  (set/union (free-type-vars input) (free-type-vars output)))

(defmethod free-type-vars :s-var [{:keys [sym]}] #{sym})

(defmethod free-type-vars :scheme
  [{:keys [s-vars body]}]
  (set/difference (free-type-vars body) (set s-vars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn free-type-vars-env
  [env]
  (if (empty? env)
    #{}
    (reduce #(set/union %1 (free-type-vars (val %2))) #{} env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @todo Consider generic substitution function (ie. clojure.walk/postwalk-replace) that replaces more than s-vars.

(defmulti substitute (fn [_ x] (if (ground? x) :ground (:type x))))

(defmethod substitute :ground [_ schema]
  (update schema :type #(get g/canonical-ground % %)))

(defmethod substitute :=>
  [subs {:keys [input output]}]
  {:type   :=>
   :input  (substitute subs input)
   :output (substitute subs output)})

(defmethod substitute :s-var
  [subs s-var]
  (loop [{:keys [sym] :as s-var} s-var]
    (if (contains? subs sym)
      (recur (get subs sym))
      s-var)))

(defn- substitute-ctor1
  [subs {:keys [child] :as schema}]
  (assoc schema :child (substitute subs child)))

(defmethod substitute :vector [subs schema] (substitute-ctor1 subs schema))
(defmethod substitute :set [subs schema] (substitute-ctor1 subs schema))
(defmethod substitute :sequential [subs schema] (substitute-ctor1 subs schema))
(defmethod substitute :maybe [subs schema] (substitute-ctor1 subs schema))

(defn- substitute-ctorN
  [subs {:keys [children] :as schema}]
  (assoc schema :children (mapv #(substitute subs %) children)))

(defmethod substitute :tuple [subs schema] (substitute-ctorN subs schema))
(defmethod substitute :cat [subs schema] (substitute-ctorN subs schema))

(defmethod substitute :map-of
  [subs {:keys [key value] :as map-of}]
  (assoc map-of
    :key (substitute subs key)
    :value (substitute subs value)))

(defmethod substitute :scheme
  [subs {:keys [s-vars body] :as scheme}]
  (assoc scheme :body (substitute (apply dissoc subs s-vars) body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn substitute-env
  [subs env]
  (->> env
       (map (fn [[sym schema]]
              [sym (substitute subs schema)]))
       (into {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compose-substitutions
  "Combines 2 sets of type substitutions."
  [subs1 subs2]
  (merge subs2
         (into {}
               (map (fn [[x schema]]
                      [x (substitute subs2 schema)])
                    subs1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti instantiate :type)

(defmethod instantiate :scheme
  [{:keys [s-vars body]}]
  (let [fresh-vars (repeatedly (count s-vars) (fn [] {:type :s-var :sym (gensym "s-")}))
        subs (zipmap s-vars fresh-vars)]
    (substitute subs body)))

(defmethod instantiate :default [schema] schema)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generalize
  [env schema]
  (let [schema (instantiate schema)
        s-vars (set/difference (free-type-vars schema) (free-type-vars-env env))]
    (if (empty? s-vars)
      schema
      {:type   :scheme
       :s-vars (vec (sort s-vars))
       :body   schema})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Most General Unifier

(defn- mgu-dispatch
  [{a-type :type :as a} {b-type :type :as b}]
  (cond
    (and (= a-type :maybe)
         (= b-type :maybe))
    [:maybe :maybe]

    (= a-type :s-var) [:s-var :_]
    (= b-type :s-var) [:_ :s-var]
    :else [a-type b-type]))

(defn mgu-failure?
  [x]
  (and (map? x) (some? (:mgu-failure x))))

(defmulti mgu mgu-dispatch)

(defn- with-mgu
  [schema1 schema2 fn]
  (let [result (mgu schema1 schema2)]
    (if (mgu-failure? result)
      result
      (fn result))))

(defn- bind-var
  [{:keys [sym] :as s-var} schema]
  (cond
    (= s-var schema) {}

    (contains? (free-type-vars schema) sym)
    {:mgu-failure :occurs-check
     :schema-1    s-var
     :schema-2    schema}

    :else {sym schema}))

(defmethod mgu [:s-var :_] [a b] (bind-var a b))
(defmethod mgu [:_ :s-var] [a b] (bind-var b a))

(defn- mgu-schema-ctor1
  [{a-type :type a-child :child :as a} {b-type :type b-child :child :as b}]
  (if (not= a-type b-type)
    {:mgu-failure :mismatched-schema-ctor
     :schema-1    a
     :schema-2    b}
    (mgu a-child b-child)))

(defmethod mgu [:vector :vector] [a b] (mgu-schema-ctor1 a b))
(defmethod mgu [:set :set] [a b] (mgu-schema-ctor1 a b))
(defmethod mgu [:sequential :sequential] [a b] (mgu-schema-ctor1 a b))
(defmethod mgu [:maybe :maybe] [a b] (mgu-schema-ctor1 a b))

(defn- mgu-schema-ctorN
  [{a-type :type a-children :children :as a}
   {b-type :type b-children :children :as b}]
  (cond
    (not= a-type b-type)
    {:mgu-failure :mismatched-schema-ctor
     :schema-1    a
     :schema-2    b}

    (not= (count a-children) (count b-children))
    {:mgu-failure :mismatched-arity
     :schema-1    a
     :schema-2    b}

    :else
    (->> (map vector a-children b-children)
         (reduce (fn [subs [a-child b-child]]
                   (if (mgu-failure? subs)
                     subs
                     (with-mgu (substitute subs a-child)
                               (substitute subs b-child)
                               #(compose-substitutions subs %))))
                 {}))))

(defmethod mgu [:tuple :tuple] [a b] (mgu-schema-ctorN a b))
(defmethod mgu [:cat :cat] [a b] (mgu-schema-ctorN a b))

(defmethod mgu [:map-of :map-of]
  [{a-key :key a-value :value} {b-key :key b-value :value}]
  (with-mgu a-key b-key
            (fn [key-subs]
              (with-mgu (substitute key-subs a-value)
                        (substitute key-subs b-value)
                        (fn [value-subs]
                          (compose-substitutions key-subs value-subs))))))

(defmethod mgu [:=> :=>]
  [{a-input :input a-output :output :as a} {b-input :input b-output :output :as b}]
  ;; @todo Support other function args (named, variatic) aside from :cat
  (if (or (not= (:type a-input) :cat)
          (not= (:type b-input) :cat))
    {:mgu-failure :non-positional-args
     :schema-1    a
     :schema-2    b}
    (with-mgu a-input b-input
              (fn [subs]
                (with-mgu (substitute subs a-output)
                          (substitute subs b-output)
                          #(compose-substitutions subs %))))))

(defmethod mgu :default
  [a b]
  (if (= a b)
    {}
    {:schema-1    a
     :schema-2    b
     :mgu-failure :non-equal}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sub-schema

;; @todo Add support for 'any?

(defn- sub-schema?-sub-dispatch
  [{:keys [type] :as schema}]
  (cond
    (class? type) :class
    (ground? schema) :ground
    :else type))

(defn- sub-schema?-dispatch
  [sub sup]
  [(sub-schema?-sub-dispatch sub)
   (sub-schema?-sub-dispatch sup)])

(defmulti sub-schema? sub-schema?-dispatch)

(defmethod sub-schema? :default
  [sub sup]
  (throw (ex-info "sub-schema? not yet supported for non-class schemas."
                  {:sub sub :sup sup})))

(defmethod sub-schema? [:class :class]
  [{sub-type :type} {sup-type :type}]
  (contains? (supers sub-type) sup-type))
