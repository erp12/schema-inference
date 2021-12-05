# Schema Inference

An experiment in implementing a Hindley–Milner inference framework for Malli schemas, as
well as other functionality for reasoning about the relationships between schemas at runtime.

> WARNING: This is not (currently) a fully fleshed out library, but rather a 
> proof-of-concept intended to foster discussion in the community.

## Motivation & Prior Art

Clojure has been using schema/spec tools to describe the data flowing through our applications, and the 
transformations which manipulate this data. The Clojure ecosystem has no shortage of schema/spec/type systems,
which [clojure.spec](https://clojure.org/about/spec), [malli](https://github.com/metosin/malli),
[plumatic schema](https://github.com/plumatic/schema), and [typed clojure](https://github.com/typedclojure/typedclojure)
being the most popular.

Most of these projects are primarily meant to be used at runtime to validate data. The exception is Typed Clojure, 
which provides a type system that is used to analyze code and flag errors before runtime.

The short feedback loop of detecting errors via static code analysis is hugely beneficial for programmer productivity.
The Clojure community has historically accepted not having this ability in favor of a different kind of short feedback
loop; the repl. The Typed Clojure project is evidence that we can have both!

But should the systems for validate data at runtime be different from the system that analyzes our code? Like
any good lisp, Clojure embraces meta-programming and code generation. If human programmers are guided by code analysis,
could we also guide our code generation with analysis at runtime?

I would love to see the expressive schema/spec tools, like clojure.spec and Malli, used to annotate code for 
development-time code analysis in addition to their use as first-class objects are runtime. This repository
contains a prototype of such a system.

First, we will expand on Malli schemas with constructs found in type systems such that a wider set of Clojure code
can be annotated and reasoned about. Second, we show that common type system operations, such as type checking
and type inference, can be performed over Clojure code that has been annotated with Malli schemas.

The goal of this project is to encourage discussion in this area and get community feedback. Eventually there may
be a "production ready" library for this purpose, managed under this repository or (ideally) integrated into 
someone else's existing project.

### Goals

1. Formalize Malli schemas into the theoretical constructs of type theory.
2. Introduce the concept of parameterized schema "schemes".
3. Implement schema-inference that can catch errors at development time.
4. Provide functionality to reason about and compose schemas at runtime.

### Non-Goals

1. Create yet another type/spec/schema system for Clojure. Stand on the shoulders of Malli.
2. Create a static code analyzer. Instead, this work might be used by an existing static code analyzer.

## Malli Schemas as "types"

**Atomic Schemas** - Malli uses predicates as atomic schemas. These predicates can be aliased as a keyword in the
registry, but we won't be considering this feature.

**Type Constructors** - Some schemas are logically a compositions other schemas. For example, `[:vector int?]` and
`[:map-of string? double?]`. We can think of `:vector` and `:map` as schema constructors that take some number 
of schemas as arguments to return a new concrete schema.

Function schemas (`[:=> [:cat int?] string?]`) can be thought of as instances of a special schema constructor, `:=>`,
which is a slightly different syntax.

## Current Features

> ANOTHER WARNING: All of these features are probably buggy and incomplete in the current prototype.
> The primary goal is to get feedback. Please report any bugs on via Github issues.

### Parametric Schema Polymorphism

What is the logical schema for the `identity` function? One might be tempted to say 
`[:=> [:cat any?] any?` but that is not correct. If it is given a value of schema `S`, it 
will _always_ return a value of schema `S`. Thus, `identity` is a polymorphic function;
its concrete schema is potentially different at every calling location.

This project proposes the construct of a schema "scheme" (terminology borrowed from the
[Hindley–Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)).
They are parameterized schemas that can be used to describe values that 
are polymorphic with respect to their schema (for example, functions like `identity`).

Let's look at the scheme of the `identity` function...

```clojure
{:s-vars ['T]
 :body [:=> [:cat [:s-var 'T]] [:s-var 'T]]}
```

This says: "for all possible values of a schema parameter `T` there is a concrete schema  
of a function that takes 1 argument of schema `T` and will return a value of schema `T`".

Notice that there is something that looks like a malli function schema (`[:=> [:cat ...] ...]`) but it contains
something new: A schema variable (`[:s-var T]`) that must be bound to a real schema in order
to turn the entire scheme into a valid schema.

We can get a concrete schema from a parametric scheme by supplying bindings for all the type variables.

```clojure
(use 'erp12.schema-inference.api)

(concretize {:s-vars ['T]
             :body [:=> [:cat [:s-var 'T]] [:s-var 'T]]}
            [int?])
;; [:=> [:cat int?] int?]
```

Common [extensions of the Hindley-Milner type system](https://users.cs.fiu.edu/~smithg/papers/thesis91.pdf)
allow schemes to have constraints. For example, the schema for a function which compares vectors might be as follows:

```clojure
{:s-vars ['A 'B]
 :with [[:= 'element-cmp [:=> [:cat [:s-var 'B] 
                                    [:s-var 'B]] 
                              int?]]
        [:< [:s-var 'A] [:s-var 'B]]]
 :body [:=> [:cat [:vector [:s-var 'A]]
                  [:vector [:s-var 'A]]]
            int?]}
```

The constraints of the above schema can be read as:
1. The `element-cmp` symbol must be bound to a binary function from 2 `B` values to an int.
2. `A` must be a sub-schema (like subtype) of `B`.

> WARNING: Sub-schema constraints are not yet implemented.

These parametric schemes are the logical representation for the generic functions and data structures.
As mentioned, there is currently little value in writing schemas for this kind of code. 
However, with parametric schemes we can properly annotate generic code which will allow schema information
to "flow" through our code, and enables schema inference.

### Schema Inference

Now that we can properly annotate all (or most) of our code, it is possible to run standard type-inference algorithms
across our forms to 1) understand what the structure of the output is and 2) check for any "type errors" in
the code.

```clojure
(defn bit-string [i]
  (Integer/toString i 2))

;; Create a schema environment for the function we will know about ahead of time.
;; These would likely come from user annotations.
(def env
  [[:= '+ [:=> [:cat int? int?] int?]]
   [:= 'inc [:=> [:cat int?] int?]]
   [:= 'bit-string [:=> [:cat int?] string?]]])

;; Run type inference on a form.
(infer-schema env '(bit-string (+ 1 (inc 1))))
;; :string

;; Let's try on bad piece of bad code.
(infer-schema env '(+ (bit-string 1) 2))
;; clojure.lang.ExceptionInfo: Types clojure.core$int_QMARK_@14debf2b and clojure.core$string_QMARK___5427@6cce16f4 do not unify.
;; {:unification-failure :standard, 
;;  :schema-1 #object[clojure.core$int_QMARK_ 0x14debf2b "clojure.core$int_QMARK_@14debf2b"],
;;  :schema-2 #object[clojure.core$string_QMARK___5427 0x6cce16f4 "clojure.core$string_QMARK___5427@6cce16f4"]}
```

The error message could use some work! ... but hopefully you get the idea.

Of course, schema-inference can help us understand individual calls to polymorphic functions that are annotated
with parametric schemes. 

```clojure
;; Annotate the `identity` symbol as having a parametric schema.
(def env
  [[:= 'bit-string [:=> [:cat int?] string?]]
   [:= 'rand-int [:=> [:cat int?] int?]]
   [:= 'identity {:s-vars ['A]
                  :body [:=> [:cat [:s-var 'A]] [:s-var 'A]]}]])

(infer-schema env '(identity 1))
;; int?

(infer-schema env '(identity :hello))
;; keyword?

(infer-schema env '(identity nil))
;; nil?

(infer-schema env '((identity bit-string) (inc 9)))
;; string?

(infer-schema env '((identity bit-string) (identity ((identity inc) 9))))
;; string?
```

Type inference of local variables, via `let`, is also supported.

```clojure
(infer-schema env
            '(let [x (+ 1 (rand-int 100))]
               x))
;; int?

(infer-schema '(let [x (+ 1 (rand-int 100))
                     y (bit-string x)]
                 y))
;; string?
```

THIS IS CAN BE LEVERAGED BY EDITORS TO 
1. DISPLAY MORE INFORMATION ABOUT INPUTS AND OUTPUTS OF A BLOCK OF CODE.
2. IDENTIFY THE LOCATION ERRORS WITH DETAILED DESCRIPTION OF THE PROBLEM.

## Potential Future Features

If this concept were to be pursued further, there are a number of useful features that I would like to see
implemented.

### Function Overloading

Sometimes we use multiple signatures to denote a polymorphic function. 

```clojure
;; NOT CURRENTLY WORKING CODE

;; Register multiple signatures for the + function.
(annotate! #'+
           [[:=> [:cat int? int?] int?]
            [:=> [:cat int? float?] float?]
            [:=> [:cat float? int?] float?]
            [:=> [:cat float? float?] float?]])

(infer-schema '(+ 1.2 34))
;; [:type float?]
```

### Schema Compatibility

Schemas are _not_ types, but up until this point we have been treating them as equivalent constructs. 
Things diverge when we ask "Do all values of schema X conform to schema Y?" or in other words "Is schema X a
sub-schema of schema Y?".

In many type systems, this would be answered by checking if type "X" extends the class "Y" or 
implements the interface "Y".

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
2. For all keys shared by the sub and super schemas, the field's schema according to the sub-schema is a sub-schema
    of the field's schema super-schema.

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

If we have a sufficiently robust implementation of schema compatibility checking, we also have a naive 
implementation of schema equivalence via `(and (sub-schema? a b) (sub-schema? b a))`.
