# Schema Inference

An experiment in implementing a Hindley–Milner inference framework for Malli schemas, as well as other functionality for
reasoning about the relationships between schemas at runtime.

> WARNING: This is not (currently) a fully fleshed out library, but rather a
> proof-of-concept intended to foster discussion in the community.

## Motivation & Prior Art

Clojure has been using schema/spec tools to describe the data flowing through our applications, and the transformations
which manipulate this data. The Clojure ecosystem has no shortage of schema/spec/type systems,
with [clojure.spec](https://clojure.org/about/spec), [malli](https://github.com/metosin/malli),
[plumatic schema](https://github.com/plumatic/schema), and [typed clojure](https://github.com/typedclojure/typedclojure)
being the most popular.

Most of these projects are primarily meant to be used at runtime to validate data. The exception is Typed Clojure, which
provides a type system that is used to analyze code and flag errors at development time.

The short feedback loop of detecting errors via static code analysis is hugely beneficial for programmer productivity.
The Clojure community has historically accepted having less of this ability in favor of a different kind of short
feedback loop; the repl. Again, there are notable exceptions such as Typed Clojure and clj-kondo.

This project hopes to demonstrate that the same schema declarations used at runtime can be used during static analysis
using a few minor extensions inspired by type theory. However, this project aim to increase the amount of thinking in
terms of types, or schemas.

**Type systems have historically been concerned with proving "correctness".** Clojurists typically agree that in a
data-oriented world type safety is often a poor approximation of correctness, and the constructs required to prove
comprehensive type safety introduce coupling and complexity that we prefer to avoid in our abstraction.

With respect to developer productivity, **type systems excel when they prove incorrectness**. Much like a
"gradual typing" system, this project aims to put Clojure's existing à la carte schema/spec constructs to use at
development time for faster, and richer, developer feedback.

### Goals

3. Introduce the concept of parameterized schema "schemes".
4. Implement schema-inference that can catch errors in Clojure forms.
5. Provide functionality to reason about and compose schemas.

### Non-Goals

1. Create yet another type/spec/schema system for Clojure. Stand on the shoulders of Malli.
2. Create a full static code analysis tool. Instead, this work might be used by an existing static code analyzer.

## Malli Schemas as "types"

This project uses malli's [map-syntax](https://github.com/metosin/malli#map-syntax) to represent schemas.

**Ground Schemas** - Any predicate is a valid ground (aka atomic) schema, however a commonly used set of schemas are
also given symbol aliases. For example `{:type 'int?}`. In the current state, this project assumes all schemas are
compositions of the ground schemas with given symbol aliases.

**Schema Constructors** - Some schemas are logically a compositions other schemas. For example, we can think of
`:vector` and `:map-of` as schema constructors (or "type constructors" from type theory) that take some number of
schemas as arguments to return a new concrete schema.

```clojure
{:type  :vector,
 :child {:type 'int}}

{:type  :map-of,
 :key   {:type :string?},
 :value {:type :double?}}
```

Function schemas can be thought of as instances of the schema constructor `:=>`.

```clojure
{:type   :=>
 :input  {:type     :cat
          :children [{:type 'string?}]}
 :output {:type 'int?}}
```

## Current Features

> ANOTHER WARNING: All of these features are probably buggy and incomplete in the current prototype.
> The primary goal is to get feedback. Please report any bugs on via Github issues.

### Parametric Schema Polymorphism

What is the logical schema for the `identity` function? One might be tempted to say
`[:=> [:cat any?] any?` but that is not correct. If it is given a value of schema `S`, it will _always_ return a value
of schema `S`. Thus, `identity` is a polymorphic function; its concrete schema is potentially different at every calling
location.

This project proposes the construct of a schema "scheme" (terminology borrowed from the
[Hindley–Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)). They are parameterized
schemas that can be used to describe values that are polymorphic with respect to their schema (for example, functions
like `identity`).

Let's look at the scheme of the `identity` function...

```clojure
{:type   :scheme
 :s-vars ['T]
 :body   {:type   :=>
          :input  {:type     :cat
                   :children [{:type :s-var, :sym 'T}]}
          :output {:type :s-var, :sym 'T}}}
```

This says: "for all possible values of a schema parameter `T` there is a concrete schema  
of a function that takes 1 argument of schema `T` and will return a value of schema `T`".

Notice that there is something that looks like a malli function schema (`{:type :=>, ...}`) but it contains something
new: A schema variable (`{:type :s-var, :sym 'T}`) that must be bound to a real schema in order to turn the entire
scheme into a concrete schema.

We can get a concrete schema from a parametric scheme by supplying bindings for all the type variables.

```clojure
(use 'erp12.schema-inference.api)

(concretize {'T {:type 'int?}}
            {:type   :scheme
             :s-vars ['T]
             :body   {:type   :=>
                      :input  {:type     :cat
                               :children [{:type :s-var, :sym 'T}]}
                      :output {:type :s-var, :sym 'T}}})
;;{:type   :=>, 
;; :input  {:type :cat, :children [{:type int?}]}, 
;; :output {:type int?}}
```

Common [extensions of the Hindley-Milner type system](https://users.cs.fiu.edu/~smithg/papers/thesis91.pdf)
allow schemes to have constraints. For example, the schema for `cons` might be as follows:

```clojure
{:type   :scheme
 :s-vars ['A 'B]
 ;; A vector of constraints. In this case only 1.
 :with   [{:type   :sub-type
           :child  {:type :s-var, :sym 'A}
           :parent {:type :s-var, :sym 'B}}]
 :body   {:type   :=>,
          :input  {:type     :cat,
                   :children [{:type :s-var, :sym 'B}
                              {:type :vector, :child {:type :s-var, :sym 'A}}]},
          :output {:type :vector, :child {:type :s-var, :sym 'B}}}}
```

The constraint of the above schema can be read as: `A` must be a sub-schema (like subtype) of `B`.

> WARNING: Scheme constraints are not yet implemented.

These parametric schemes are the logical representation for generic functions and data structures. There is currently
little value in writing schemas for this kind of code. However, with parametric schemes we can properly annotate generic
code which will allow schema information to "flow" through our code, and enables schema inference.

### Schema Inference

Now that we can properly annotate all (or most) of our code, it is possible to run standard type-inference algorithms
across our forms to 1) understand what the structure of the output is and 2) check for any "type errors" in the code.

This project assumes the code to type check is presented as an AST as produced
by [tools.analyzer](https://github.com/clojure/tools.analyzer).

```clojure
(require '[malli.core :as m]
         '[clojure.tools.analyzer.jvm :as ana])

(defn square [i]
  (* i i))

(defn str-length [s]
  (.length s))

;; Create a schema environment for the function we will know about ahead of time.
;; These would likely come from user annotations.
(def env
  {`square     (m/ast [:=> [:cat 'int?] 'int?])
   `str-length (m/ast [:=> [:cat 'string?] 'int?])})

;; Run type inference on a form.
(infer-schema
  (ana/analyze `(square (str-length "clojure")))
  env)
;; {:type 'int?}

;; Let's try on bad piece of bad code.
(infer-schema
  (ana/analyze `(str-length (square 2)))
  env)
;; clojure.lang.ExceptionInfo: Schema inference failure. 
;; #:erp12.schema-inference.impl.algo_w{:failure {:unification-failure {:schema-1 {:type string?}, :schema-2 {:type int?}, :mgu-failure :non-equal}}}
```

The error message could use some work! ... but hopefully you get the idea.

Of course, schema-inference can help us understand individual calls to polymorphic functions that are annotated with
parametric schemes.

```clojure
;; Annotate the `identity` symbol as having a parametric schema.
(def env
  {`square     (m/ast [:=> [:cat 'int?] 'int?])
   `str-length (m/ast [:=> [:cat 'string?] 'int?])
   `rand-int   (m/ast [:=> [:cat 'int?] 'int?])
   `identity   {:type   :scheme
                :s-vars ['a]
                :body   {:type   :=>
                         :input  {:type     :cat
                                  :children [{:type :s-var :sym 'a}]}
                         :output {:type :s-var :sym 'a}}}})

(infer-schema
  (ana/analyze '(identity 1))
  env)
;; {:type 'int?}

(infer-schema
  (ana/analyze '(identity :hello))
  env)
;; {:type 'keyword?}

(infer-schema
  (ana/analyze '((identity square) (str-length "malli")))
  env)
;; {:type 'int?}

```

Type inference of local variables, via `let`, is also supported.

```clojure
(infer-schema
  (ana/analyze '(let [x (rand-int 100)]
                  x))
  env)
;; {:type 'int?}
```

## Potential Future Features

If this concept were to be pursued further, there are a number of useful features that I would like to see implemented.

### Function Overloading

Sometimes we use multiple signatures to denote a polymorphic function.

```clojure
;; NOT CURRENTLY WORKING CODE

(def env
  ;; The `+` variable has a collection of type annotations.
  {'+ [(m/ast [:=> [:cat int? int?] int?])
       (m/ast [:=> [:cat int? float?] float?])
       (m/ast [:=> [:cat float? int?] float?])
       (m/ast [:=> [:cat float? float?] float?])]})
;; Register multiple signatures for the + function.

(infer-schema '(+ 1 2.3) env)
;; [:type float?]
```

### Schema Compatibility

Schemas are _not_ types, but up until this point we have been treating them as equivalent constructs. Things diverge
when we ask "Do all values of schema X conform to schema Y?" or in other words "Is schema X a sub-schema of schema Y?".

In many type systems, this would be answered by checking if type "X" extends the class "Y" or implements the 
interface "Y".

With schemas things are entirely structural. Let's look at some examples:

```clojure
;; NOT CURRENTLY WORKING CODE

(sub-schema? [:enum :A :C]
             [:enum :A :B :C])
;; true
```

Obviously the first schema (`[:enum :A :C]`) is a sub-schema of the second schema (`[:enum :A :B :C]`)
because the first schema's finite set of members is a subset of the second schema's members.

The same can be trivially determined for some non-enum schemas.

```clojure
;; NOT CURRENTLY WORKING CODE

(sub-schema? int?
             [:or int? float?])
;; true

(sub-schema? [:or int? float?]
             float?)
;; false

(sub-schema? keyword?
             [:and qualified-ident? keyword?])
;; false

(sub-schema? [:and qualified-ident? keyword?]
             keyword?)
;; true
```

We can extend this logic to maps by checking that:

1. All the (required) keys of the super-schema are present in the sub-schema.
2. For all keys shared by the sub and super schemas, the field's schema according to the sub-schema is a sub-schema of
   the field's schema super-schema.

```clojure
;; NOT CURRENTLY WORKING CODE

(sub-schema? [:map  ;; date-time
              [:year int?]
              [:month int?]
              [:day int?]
              [:hour int?]
              [:minute int?]
              [:second int?]]
             [:map ;; date
              [:year int?]
              [:month int?]
              [:day int?]])
;; true - All date-times have a complete set of date attributes.

(sub-schema? [:map ;; Ranked product recommendations from an ML model that outputs probabilities.
              [:product-id keyword?]
              [:rank float?]]
             [:map ;; Any numeric ranking system.
              [:product-id keyword?]
              [:rank number?]])
;; true - All probability rankings are numeric rankings.
```

We can assume that all other collections (lists, vectors, sets) are covariant with respect to their element schemas.

```clojure
;; NOT CURRENTLY WORKING CODE

(sub-schema? [:vector int?]
             [:vector number?])
;; true

(sub-schema? [:set string?]
             [:set [:maybe string?]])
;; true

(sub-schema? [:vector boolean?]
             [:sequential boolean?])
;; true
```

If we have a sufficiently robust implementation of schema compatibility checking, we also have a naive implementation of
schema equivalence via `(and (sub-schema? a b) (sub-schema? b a))`.
