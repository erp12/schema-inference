(ns erp12.schema-inference.api
  (:require [erp12.schema-inference.inference :as inf]
            [erp12.schema-inference.ast :as ast]
            [erp12.schema-inference.schema :as sch]))

(defn infer-schema
  "Infer the type of the expression given the environment."
  [env form]
  (inf/infer-schema env (ast/form->ast form)))

(defn concretize
  [scheme schema-args]
  (sch/concretize scheme schema-args))
