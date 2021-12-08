(ns erp12.schema-inference.inference-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.schema-inference.inference :refer :all]
            [clojure.core.match :refer [match]]
            [erp12.schema-inference.schema :as sch]))

(deftest infer-ground-schema-test
  (is (= int? (infer-ground-schema 1))))

(deftest infer-schema-test
  (let [env [[:= 'inc [:=> [:cat int?] int?]]
             [:= '+ [:=> [:cat int? int?] int?]]
             [:= 'map {:s-vars ['A 'B]
                       :body   [:=> [:cat [:=> [:cat [:s-var 'A]] [:s-var 'B]]
                                     [:vector [:s-var 'A]]]
                                [:vector [:s-var 'B]]]}]
             [:= 'int->str [:=> [:cat int?] string?]]]]
    (testing "literals"
      (is (= int? (infer-schema env [:lit 1]))))
    (testing "Function application"
      (is (= int? (infer-schema env [:apply [:var 'inc] [:lit 1]])))
      (is (= int? (infer-schema env [:apply [:var '+] [:lit 1] [:lit 2]]))))
    (testing "Function abstraction"
      (is (= [:=> [:cat int?] int?]
             (infer-schema env '[:fn [:cat [:var x]] [:apply [:var inc] [:apply [:var inc] [:var x]]]]))))
    (testing "Let"
      (is (= int? (infer-schema env '[:let [[:var x] [:lit 100]] [:var x]]))))
    (testing "Schemes"
      (match [(infer-schema env '[:fn [:cat [:var x]] [:var x]])]
        [{:s-vars [tv]
          :with   []
          :body   [:=> [:cat [:s-var t1]] [:s-var t2]]}]
        (do
          (is (sch/gen-s-var? tv))
          (is (= tv t1 t2)))))
    (testing "Higher Order Functions"
      (match [(infer-schema env '[:fn [:cat [:var f] [:var x]] [:apply [:var f] [:var x]]])]
        [{:s-vars [a1 b1]
          :with   []
          :body   [:=> [:cat [:=> [:cat [:s-var b2]] [:s-var a2]]
                        [:s-var b3]]
                   [:s-var a3]]}]
        (let [t-vars #{a1 b1}]
          ;; Check type scheme variables are "fresh".
          (is (sch/gen-s-var? a1))
          (is (sch/gen-s-var? b1))
          (is (not (= a1 b1)))
          ;; Check inner type vars are bound in scheme.
          (doseq [t [a2 a3 b2 b3]]
            (is (contains? t-vars t)))
          ;; Check corresponding type variables match up.
          (is (= a2 a3))
          (is (= b2 b3))))
      (is (= [:vector string?]
             (infer-schema (conj env [:= 'my-vec [:vector int?]])
                           [:apply [:var 'map]
                            [:var 'int->str]
                            [:var 'my-vec]])))
      (is (thrown? clojure.lang.ExceptionInfo
                   #"Types :float and int? do not unify\."
                   (infer-schema (conj env [:= 'my-vec [:vector float?]])
                                 [:apply [:var 'map] [:var 'int->str] [:var 'my-vec]]))))))
