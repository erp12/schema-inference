(ns erp12.schema-inference.impl.algo_w-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.tools.analyzer.jvm :as ana]
            [erp12.schema-inference.impl.algo_w :refer [algo-w] :as a])
  (:import (java.io PrintStream)))

;; Keep some empty vars to be used arbitrarily in test ASTs.
(declare f)

;; @todo Test type checker failures

(def test-env
  {'clojure.lang.Numbers/inc
   {:type   :=>
    :input  {:type     :cat
             :children [{:type 'int?}]}
    :output {:type 'int?}}

   'clojure.core/inc
   {:type   :=>
    :input  {:type     :cat
             :children [{:type 'int?}]}
    :output {:type 'int?}}

   'clojure.core/if
   {:type   :scheme
    :s-vars ['a]
    :body   {:type   :=>
             :input  {:type     :cat
                      :children [{:type 'boolean?}
                                 {:type :s-var :sym 'a}
                                 {:type :s-var :sym 'a}]}
             :output {:type :s-var :sym 'a}}}

   'clojure.core/map
   {:type   :scheme
    :s-vars ['a 'b]
    :body   {:type   :=>
             :input  {:type     :cat
                      :children [{:type   :=>
                                  :input  {:type     :cat
                                           :children [{:type :s-var :sym 'a}]}
                                  :output {:type :s-var :sym 'b}}
                                 {:type  :vector
                                  :child {:type :s-var :sym 'a}}]}
             :output {:type  :vector
                      :child {:type :s-var :sym 'b}}}}})

(deftest algo-w-const-test
  (is (= (algo-w (ana/analyze :a) test-env)
         {::a/subs   {}
          ::a/schema {:type 'keyword?}})))

(deftest algo-w-do-test
  (is (= (algo-w (ana/analyze '(do (println "!") 1)) test-env)
         {::a/subs   {}
          ::a/schema {:type 'int?}})))

(deftest algo-w-fn-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze '(fn [x] (inc x))) test-env)]
    (is (nil? failure))
    (is (= schema {:type   :=>
                   :input  {:type     :cat
                            :children [{:type 'int?}]}
                   :output {:type 'int?}}))
    (is (= (count subs) 2)))
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(fn [x#] (f (inc x#) 1)))
                (assoc test-env
                  `f {:type   :scheme
                      :s-vars ['a]
                      :body   {:type   :=>
                               :input  {:type     :cat
                                        :children [{:type :s-var :sym 'a}
                                                   {:type :s-var :sym 'a}]}
                               :output {:type :s-var :sym 'a}}}))]
    (is (nil? failure))
    (is (= schema {:type   :=>
                   :input  {:type     :cat
                            :children [{:type 'int?}]}
                   :output {:type 'int?}}))
    (is (= (count subs) 4)))
  (testing "nullary"
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze `((fn [] 1))) {})]
      (is (nil? failure))
      (is (= schema {:type 'int?}))
      (is (= (count subs) 1))))
  (testing "polymorphic"
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze `(fn [x# y#] (f x# y#)))
                  (assoc test-env
                    `f {:type   :scheme
                        :s-vars ['a 'b]
                        :body   {:type   :=>
                                 :input  {:type     :cat
                                          :children [{:type :s-var :sym 'a}
                                                     {:type :s-var :sym 'b}]}
                                 :output {:type :s-var :sym 'b}}}))
          inputs (set (get-in schema [:input :children]))
          output (:output schema)]
      (is (nil? failure))
      ;; @todo Find better way to test. Meander?
      (is (= (:type schema) :=>))
      (is (= (count inputs) 2))
      (is (contains? inputs output))
      (is (every? #(= (:type %) :s-var) (cons output inputs)))
      (is (= (count subs) 3)))))

(deftest algo-w-if-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(if true 1 2)) test-env)]
    (is (nil? failure))
    (is (= schema {:type 'int?}))
    (is (= (count subs) 2))))

(deftest algo-w-import-test
  (is (= (algo-w (ana/analyze `(import 'clojure.lang.Keyword)) test-env)
         {::a/subs {} ::a/schema nil?})))

(deftest algo-w-instance?-test
  (is (= (algo-w (ana/analyze `(instance? String "")) test-env)
         {::a/subs {} ::a/schema 'boolean?})))

(deftest algo-w-invoke-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(map inc [0])) test-env)]
    (is (nil? failure))
    (is (= schema {:type :vector :child {:type 'int?}}))
    (is (= (count subs) 3))))

(deftest algo-w-let-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(let [f# inc
                                    a# 1]
                                (f# a#)))
                test-env)]
    (is (nil? failure))
    (is (= schema {:type 'int?}))
    (is (= (count subs) 1))))

(deftest algo-w-letfn-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(letfn [(f# [x#] (inc x#))
                                      (g# [y#] (f# (f# y#)))]
                                (g# 0)))
                test-env)]
    (is (nil? failure))
    (is (= schema {:type 'int?}))
    (is (= (count subs) 6)))
  ; @todo (testing "ahead-of-definition")
  )

;; Implicitly tested
;(deftest algo-w-local-test)

(deftest algo-w-prim-invoke-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze '((fn [^long x] x) 1)) test-env)]
    (is (nil? failure))
    (is (= schema {:type 'int?}))
    (is (= (count subs) 2))))

(defprotocol P
  (foo [_ x]))

(defrecord R [y]
  P
  (foo [_ x] (+ x y)))

(deftest algo-w-protocol-invoke-test
  (let [{::a/keys [subs schema failure] :as r}
        (algo-w (ana/analyze `(foo (->R 1) 2))
                (assoc test-env
                  `->R {:type   :=>
                        :input  {:type     :cat
                                 :children [{:type 'int?}]}
                        :output {:type R}}
                  `foo {:type   :=>
                        :input  {:type     :cat
                                 :children [{:type (:on-interface P)}
                                            {:type 'int?}]}
                        :output {:type 'int?}}))]
    (is (nil? failure))
    (is (= schema {:type 'int?}))
    (is (= (count subs) 1))))

(deftest algo-w-quote-test
  (is (= (algo-w (ana/analyze `(quote (+ 1 2))) test-env)
         {::a/subs {}
          ::a/schema {:type :sequential
                      :child {:type 'some?}}})))

;(deftest algo-w-set!-test)

(deftest algo-w-static-call-test
  (let [{::a/keys [subs schema failure]} (algo-w (ana/analyze '(inc 1)) test-env)]
    (is (nil? failure))
    (is (= schema {:type 'int?}))
    (is (= (count subs) 1))))

(deftest algo-w-static-field-test
  (let [{::a/keys [subs schema failure]} (algo-w (ana/analyze `System/out) test-env)]
    (is (nil? failure))
    (is (= schema {:type PrintStream}))
    (is (= (count subs) 0))))

(deftest algo-w-the-var-test
  (let [{::a/keys [subs schema failure]} (algo-w (ana/analyze `(var +)) test-env)]
    (is (nil? failure))
    (is (= schema {:type 'var?}))
    (is (= (count subs) 0))))

;(deftest algo-w-throw-test)
;(deftest algo-w-try-test)

(deftest algo-w-var-test
  (let [{::a/keys [subs schema failure]} (algo-w (ana/analyze `clojure.core/inc) test-env)]
    (is (nil? failure))
    (is (= schema {:type :=>
                   :input {:type :cat
                           :children [{:type 'int?}]}
                   :output {:type 'int?}}))
    (is (= (count subs) 0))))