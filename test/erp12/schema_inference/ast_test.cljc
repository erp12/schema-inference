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

(deftest free-vars-test
  ;; (\x -> x (\z -> y z)) has free variables {x}
  (is (= #{'y}
         (free-vars '[:fn
                      [:cat [:var x]]
                      [:apply [:var x] [:fn [:cat [:var z]] [:apply [:var y] [:var z]]]]])))
  ;; (\x -> \y -> x y)(y w) has free variables {y w}
  (is (= #{'y 'w}
         (free-vars '[:apply
                      [:fn [:cat [:var x]] [:fn [:cat [:var y]] [:apply [:var x] [:var y]]]]
                      [:apply [:var y] [:var w]]])))
  ;; Function with multiple body statements.
  (is (= #{'println 'y}
         (free-vars '[:fn [:cat [:var x]]
                      [:apply [:var println] [:var y] [:var x]]
                      [:lit "Done"]])))
  ;; Let
  (is (= #{'f 'f2}
         (free-vars '[:let [a [:lit 1]
                            b [:apply [:var f] [:var a]]]
                      [:apply [:var f2] [:var b] [:var a]]]))))

(deftest substitute-vars-test
  (is (= '[:var z] (substitute-vars '{x y} '[:var z])))
  (is (= '[:apply [:var y] [:var y]]
         (substitute-vars '{x y} '[:apply [:var x] [:var x]])))
  (is (= '[:apply [:var z] [:var z]]
         (substitute-vars '{x y} '[:apply [:var z] [:var z]])))
  (testing "functions"
    (match (substitute-vars '{x y} '[:fn [:cat [:var x]] [:var x]])
      [:fn [:cat [:var arg]] [:var body]]
      (do
        (is (gen-var? arg))
        (is (= arg body))))
    (match (substitute-vars '{x y} '[:fn [:cat [:var y]] [:var y]])
      [:fn [:cat [:var arg]] [:var body]]
      (do
        (is (gen-var? arg))
        (is (gen-var? body))))
    (match (substitute-vars '{x y} '[:fn [:cat [:var y]] [:apply [:var println] [:var y]] [:var y]])
      [:fn [:cat [:var a]] [:apply [:var 'println] [:var b]] [:var c]]
      (do
        (is (gen-var? a))
        (is (= a b c))))))
