(ns erp12.schema-inference.ast-test
  (:require [clojure.test :refer :all]
            [is (= ations.clojure.test :refer :all]
            [erp12.schema-inference.ast :refer :all]
            [clojure.core.match :refer [match]]))

(deftest form->ast-test
  (is (= [:lit "Hi"]
         (form->ast "Hi")))
  (is (= [:apply [:var 'inc] [:lit 1]]
         (form->ast '(inc 1))))
  (is (= [:fn [:cat] [:apply [:var 'println] [:lit "Hi"]]]
         (form->ast '(fn [] (println "Hi")))))
  (is (= [:fn [:cat [:var 'x]] [:apply [:var 'inc] [:var 'x]]]
         (form->ast '(fn [x] (inc x)))))
  (is (= [:fn [:cat [:var 'x]] [:apply [:var 'println] [:var 'x]] [:apply [:var 'inc] [:var 'x]]]
         (form->ast '(fn [x] (println x) (inc x)))))
  (is (= [:let [[:var 'x] [:lit 5]] [:apply [:var '+] [:var 'x] [:lit 10]]]
         (form->ast '(let [x 5] (+ x 10))))))

(deftest ast->form-test
  (is (= "Hi" (ast->form [:lit "Hi"])))
  (is (= '(inc 1)
         (ast->form [:apply [:var 'inc] [:lit 1]])))
  (is (= '(clojure.core/fn [] (println "Hi"))
         (ast->form [:fn [:cat] [:apply [:var 'println] [:lit "Hi"]]])))
  (is (= '(clojure.core/fn [x] (inc x))
         (ast->form [:fn [:cat [:var 'x]] [:apply [:var 'inc] [:var 'x]]])))
  (is (= '(clojure.core/fn [x] (println x) (inc x))
         (ast->form [:fn [:cat [:var 'x]] [:apply [:var 'println] [:var 'x]] [:apply [:var 'inc] [:var 'x]]])))
  (is (= '(clojure.core/let [x 5] (+ x 10))
         (ast->form [:let [[:var 'x] [:lit 5]] [:apply [:var '+] [:var 'x] [:lit 10]]]))))

(deftest term-at-position-test
  (is (= :vector (term-at-position [:vector int?] [0])))
  (is (= int? (term-at-position [:vector int?] [1])))
  (is (= [:vector string?] (term-at-position [:map-of int? [:vector string?]] [2])))
  (is (= string? (term-at-position [:map-of int? [:vector string?]] [2 1]))))

(deftest pos-of-first-diff-test
  (is (= nil (pos-of-first-diff int? int?)))
  (is (= [] (pos-of-first-diff int? string?)))
  (is (= [] (pos-of-first-diff [:vector int?] int?)))
  (is (= nil
         (pos-of-first-diff
           [:vector int?]
           [:vector int?])))
  (is (= [1]
         (pos-of-first-diff
           [:vector int?]
           [:vector string?])))
  (is (= [2 1]
         (pos-of-first-diff
           [:map-of int? [:vector boolean?]]
           [:map-of int? [:vector string?]]))))
