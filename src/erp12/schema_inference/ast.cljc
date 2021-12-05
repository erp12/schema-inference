(ns erp12.schema-inference.ast
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as w]
            [malli.core :as m]
            [malli.util :as mu]))

(defn var-form?
  [form]
  (and (symbol? form)
       (not (keyword? form))))

(defn abs-form?
  [form]
  (and (list? form)
       (= (first form) 'fn)))

(defn let-form?
  [form]
  (and (list? form)
       (= (first form) 'let)))

(defn app-form?
  [form]
  (and (list? form)
       (not (abs-form? form))
       (not (let-form? form))))

(defn form->ast
  [form]
  (cond
    (var-form? form)
    [:var form]

    (app-form? form)
    (vec (cons :apply (map form->ast form)))

    (abs-form? form)
    (let [arg-vars (vec (map form->ast (second form)))
          body (map form->ast (drop 2 form))]
      (vec (concat [:fn (vec (cons :cat arg-vars))] body)))

    (let-form? form)
    (let [var-&-def (->> form second (map form->ast) vec)
          body (map form->ast (drop 2 form))]
      (vec (concat [:let var-&-def] body)))

    :else
    [:lit form]))

(defn ast->form
  [ast]
  (w/postwalk (fn [node]
                (match node
                  [:lit v]
                  v

                  [:var s]
                  s

                  [:apply f & args]
                  (cons f args)

                  [:fn [:cat & args] & body]
                  `(fn [~@args] ~@body)

                  [:let [& vars-&-defs] & body]
                  `(let ~(vec vars-&-defs) ~@body)

                  :else node))
              ast))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST Schemas

;; Expressions

(def Lit
  [:tuple [:= :lit] :any])

(def Var
  [:tuple [:= :var] :symbol])

(def Abs
  [:catn
   [:_ [:= :fn]]
   [:args [:cat [:= :cat] [:* [:schema [:ref ::AST]]]]]
   [:ret [:schema [:ref ::AST]]]])

(def App
  [:catn
   [:_ [:= :apply]]
   [:func [:schema [:ref ::AST]]]
   [:args [:* [:schema [:ref ::AST]]]]])

(def Let
  [:catn
   [:_ [:= :let]]
   [:vars [:schema [:* [:cat simple-symbol? [:schema [:ref ::AST]]]]]]
   [:body [:* [:schema [:ref ::AST]]]]])

(def Expr
  [:orn
   [:lit [:schema [:ref ::Lit]]]
   [:var [:schema [:ref ::Var]]]
   [:abs [:schema [:ref ::Abs]]]
   [:app [:schema [:ref ::App]]]
   [:let [:schema [:ref ::Let]]]])

;; Types (AKA Schemas)

(def Mono
  [:and [:schema [:ref ::Schema]] [:not [:schema [:ref ::Scheme]]]])

(def Atomic
  [:or keyword? fn?])

(def SVar
  [:tuple [:= :s-var] :symbol])

(def Fn
  [:catn
   [:_ [:= :=>]]
   [:args [:schema [:cat [:= :cat] [:* Mono]]]]
   [:ret Mono]])

(def Ctor
  [:catn
   [:ctor keyword?]
   [:* Mono]])

(def Scheme
  [:map
   [:s-vars
    [:vector symbol?]]
   [:with {:optional true}
    [:vector [:schema [:ref ::Assumption]]]]
   [:body Mono]])

(def Schema
  [:orn
   [:atomic [:schema [:ref ::Atomic]]]
   [:s-var [:schema [:ref ::SVar]]]
   [:fn [:schema [:ref ::Fn]]]
   [:ctor [:schema [:ref ::Ctor]]]
   [:scheme [:schema [:ref ::Scheme]]]])

;; Environment / Constraints / Assumptions

(def Typing
  [:tuple [:= :=] symbol? [:schema [:ref ::Schema]]])

(def Subtyping
  [:tuple [:= :<] [:schema [:ref ::Schema]] [:schema [:ref ::Schema]]])

(def Assumption
  [:orn
   [:typing [:schema [:ref ::Typing]]]
   [:subtyping [:schema [:ref ::Subtyping]]]])

(def Env
  [:vector [:schema [:ref ::Assumption]]])

(def Subs
  [:map-of [:schema [:ref ::Var]] [:schema [:ref ::Schema]]])

;; API

(def AST
  [:orn
   [:expr [:schema [:ref ::Expr]]]
   [:schema [:schema [:ref ::Schema]]]])

(def registry
  (merge (m/default-schemas)
         (mu/schemas)
         {;; Expression
          ::Lit        Lit
          ::Var        Var
          ::Abs        Abs
          ::App        App
          ::Let        Let
          ::Expr       Expr
          ;; Schema
          ::Atomic     Atomic
          ::SVar       SVar
          ::Fn         Fn
          ::Ctor       Ctor
          ::Scheme     Scheme
          ::Schema     Schema
          ;; Environment
          ::Typing     Typing
          ::Subtyping  Subtyping
          ::Assumption Assumption
          ::Env        Env
          ;; Other
          ::Subs       Subs
          ::AST        AST}))


(defn atomic?
  [ast]
  (m/validate ::Atomic ast {:registry registry}))

(defn scheme?
  [ast]
  (m/validate ::Scheme ast {:registry registry}))

(defn env?
  [x]
  (m/validate ::Env x {:registry registry}))

(defn occurs?
  [x ast]
  (->> ast
       (tree-seq coll? #(if (map? %) (vals %) %))
       (filter #(= x %))
       empty?
       not))

(defn term-at-position
  [ast position]
  (if (atomic? ast)
    (if (empty? position)
      ast
      nil)
    (reduce #(nth %1 %2 nil) ast position)))

(defn pos-of-first-diff
  [s1 s2]
  (loop [pos []]
    (let [t1 (term-at-position s1 pos)
          t2 (term-at-position s2 pos)]
      (cond
        ;; If terms are equal, move to next term.
        (and (some? t1) (= t1 t2))
        (recur (if (empty? pos)
                 [0]
                 (update pos (dec (count pos)) inc)))

        ;; If both terms have children, and start with the same ctor, go deeper.
        (and (vector? t1)
             (vector? t2)
             (= (first t1) (first t2)))
        (recur (conj pos 0))

        ;; If diff found, return pos
        (not= t1 t2)
        pos

        ;; If reached all terms for both, schemas are the same.
        :else nil))))
