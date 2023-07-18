(ns erp12.schema-inference.api
  (:require [erp12.schema-inference.impl.algo_w :as algo-w]))

(defn infer-schema
  "Infer the schema of the expression given the environment."
  [ast env]
  (algo-w/infer-schema ast env))
