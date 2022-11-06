(ns erp12.schema-inference.api
  (:require [erp12.schema-inference.impl.algo_w :as algo-w]
            [erp12.schema-inference.impl.util :as u]))

(defn infer-schema
  "Infer the schema of the expression given the environment."
  [ast env]
  (algo-w/infer-schema ast env))

(defn concretize
  [bindings {:keys [type] :as schema}]
  (let [schema (if (= type :scheme) (:body schema) schema)]
    (->> schema
         (u/substitute bindings)
         (u/generalize {}))))


(comment

  (concretize {'T {:type 'int?}}
              {:type   :scheme
               :s-vars ['T]
               :body   {:type   :=>
                        :input  {:type     :cat
                                 :children [{:type :s-var, :sym 'T}]}
                        :output {:type :s-var, :sym 'T}}})

  )