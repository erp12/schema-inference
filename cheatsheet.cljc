(ns cheatsheet)

;; Cheatsheet for the DSL/AST representation used throughout
;; this project.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions / AST

;; Constant
[:lit 1]
[:lit "Hello world"]

;; Variable
[:var 's]

;; Abstraction
[:fn [:cat 'a1 'a2] '& 'body]

;; Application
[:apply 'funct '& 'args]

;; Let
[:let ['x 'e1] '& 'body]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schemas

;; Schema Variable
[:s-var 'A]

;; Ground Schemas - Atomic Malli schemas (or predicates)
int?
string?

;; Schema Constructor - Composite Malli schemas
[:vector :int]
[:map-of :string :boolean]

;; Function Schema - From Malli. A special case of schema constructor
[:=> [:cat 'arg-type-1 'arg-type-2] 'ret-type]

;; Schema Scheme - Parametric polymorphism
{:s-vars ['T 'U]
 :with [[:= '+ [:=> [:cat [:s-var 'T] [:s-var 'T]] [:s-var 'T]]]
        [:< [:s-var 'T] [:s-var 'U]]]
 :body [:=> [:cat [:s-var 'T]] [:s-var 'U]]}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment, Assumptions, Substitutions

;; Typing Assumption
[:= 'x :int]
[:= 'y [:vector :string]]
[:= 'z {:s-vars ['a] :body [:=> [:cat [:s-var 'a]] [:s-var 'a]]}]

;; Subtyping Assumption
[:< :int :number]

;; Environment
[& assumptions]

;; Substitutions - Map of schema variables to schemas.
{'T :int}
