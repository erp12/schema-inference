(ns erp12.schema-inference.impl.util-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [erp12.schema-inference.impl.util :as u]))

(deftest ground?-test
  (is (u/ground? {:type 'string?}))
  (is (not (u/ground? {:type  :vector
                       :child {:type 'int?}})))
  (is (not (u/ground? {:type   :=>
                       :input  {:type     :cat
                                :children [{:type 'int?}]}
                       :output {:type 'float?}})))
  (is (not (u/ground? {:type   :scheme
                       :s-vars ['x]
                       :body   {:type   :=>
                                :input  {:type     :cat
                                         :children [{:type 'int?}]}
                                :output {:type 'float?}}}))))

(deftest substitute-test
  (let [x->y #(u/substitute {'x {:type :s-var :sym 'y}} %)]
    (is (= {:type :s-var :sym 'y}
           (x->y {:type :s-var :sym 'x})))
    (is (= {:type :s-var :sym 'z}
           (x->y {:type :s-var :sym 'z})))
    (testing "tuple schema"
      (is (= {:type :tuple :children [{:type :s-var :sym 'y} {:type :s-var :sym 'y}]}
             (x->y {:type :tuple :children [{:type :s-var :sym 'x} {:type :s-var :sym 'x}]}))))
    (testing "function schema"
      (is (= {:type   :=>
              :input  {:type     :cat
                       :children [{:type :s-var :sym 'y}]}
              :output {:type :s-var :sym 'y}}
             (x->y {:type   :=>
                    :input  {:type     :cat
                             :children [{:type :s-var :sym 'x}]}
                    :output {:type :s-var :sym 'x}}))))
    (testing "scheme"
      (is (= {:type   :scheme
              :s-vars ['z]
              :body   {:type :s-var :sym 'y}}
             (x->y {:type   :scheme
                    :s-vars ['z]
                    :body   {:type :s-var :sym 'x}})))
      ;; Occurs check
      (is (= {:type   :scheme
              :s-vars ['x]
              :body   {:type :s-var :sym 'x}}
             (x->y {:type   :scheme
                    :s-vars ['x]
                    :body   {:type :s-var :sym 'x}}))))))

(deftest substitute-env-test
  (is {'a {:type   :scheme
           :s-vars ['z]
           :body   {:type  :vector
                    :child {:type :s-var :sym 'y}}}
       'b {:type   :scheme
           :s-vars ['x]
           :body   {:type  :set
                    :child {:type :s-var :sym 'x}}}}
      (u/substitute-env {'x {:type :s-var :sym 'y}}
                        {'a {:type   :scheme
                             :s-vars ['z]
                             :body   {:type  :vector
                                      :child {:type :s-var :sym 'x}}}
                         'b {:type   :scheme
                             :s-vars ['x]
                             :body   {:type  :set
                                      :child {:type :s-var :sym 'x}}}})))

(deftest compose-substitutions-test
  (is (= (u/compose-substitutions {} {})
         {}))
  (is (= (u/compose-substitutions {'a {:type :s-var, :sym 'b}}
                                  {'b {:type 'boolean?}})
         {'a {:type 'boolean?}
          'b {:type 'boolean?}}))
  (is (= (u/compose-substitutions {'x {:type 'string?}
                                   'y {:type 'int?}}
                                  {'y {:type :s-var :sym 'x}})
         {'x {:type 'string?}
          'y {:type 'int?}})))

(deftest free-type-vars-test
  (is (= #{'x} (u/free-type-vars {:type :s-var :sym 'x})))
  (is (= #{} (u/free-type-vars {:type 'string?})))
  (testing "function schemas"
    (is (= #{'x 'y} (u/free-type-vars {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym 'x}]}
                                       :output {:type :s-var :sym 'y}})))
    (is (= #{'x 'y} (u/free-type-vars {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym 'x}
                                                           {:type :s-var :sym 'y}]}
                                       :output {:type :s-var :sym 'x}}))))
  (is (= #{}
         (u/free-type-vars {:type :map-of :key {:type 'int?} :value {:type 'string?}})))
  (testing "scheme"
    (is (= #{'y}
           (u/free-type-vars {:type   :scheme
                              :s-vars ['x]
                              :body   {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym 'x}]}
                                       :output {:type :s-var :sym 'y}}})))
    (is (= #{}
           (u/free-type-vars {:type   :scheme
                              :s-vars ['x 'y]
                              :body   {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym 'x}]}
                                       :output {:type :s-var :sym 'y}}})))))

(deftest free-type-vars-env-test
  (is (= (u/free-type-vars-env {'a {:type   :scheme
                                    :s-vars ['z]
                                    :body   {:type  :vector
                                             :child {:type :s-var :sym 'x}}}
                                'b {:type   :scheme
                                    :s-vars ['x]
                                    :body   {:type  :set
                                             :child {:type :s-var :sym 'x}}}})
         #{'x})))

(deftest instantiate-test
  (is (= (u/instantiate {:type 'int?})
         {:type 'int?}))
  (is (= (u/instantiate {:type :s-var :sym 'x})
         {:type :s-var :sym 'x}))
  (let [s (u/instantiate {:type   :scheme
                          :s-vars ['x]
                          :body   {:type  :vector
                                   :child {:type :s-var :sym 'x}}})]
    (is (= (:type s) :vector))
    (is (= (get-in s [:child :type]) :s-var))
    (is (str/starts-with? (name (get-in s [:child :sym])) "s-"))))

(deftest generalize-test
  (let [env {'a {:type 'int?}
             'b {:type :s-var :sym 'x}}]
    (is (= {:type 'int?}
           (u/generalize env {:type 'int?})))
    (is (= {:type :s-var :sym 'x}
           (u/generalize env {:type :s-var :sym 'x})))
    (is (= {:type   :scheme
            :s-vars ['y]
            :body   {:type  :vector
                     :child {:type :s-var :sym 'y}}}
           (u/generalize env
                         {:type  :vector
                          :child {:type :s-var :sym 'y}})))))

(deftest mgu-test
  (testing "atomic types"
    (is (= (u/mgu {:type 'int?} {:type 'int?})
           {}))
    (is (= (u/mgu {:type 'int?} {:type 'string?})
           {:mgu-failure :non-equal
            :schema-1    {:type 'int?}
            :schema-2    {:type 'string?}})))
  (testing "s-vars"
    (is (= (u/mgu {:type :s-var :sym 'a}
                  {:type :s-var :sym 'b})
           {'a {:type :s-var :sym 'b}}))
    (is (= (u/mgu {:type 'int?}
                  {:type :s-var :sym 'a})
           {'a {:type 'int?}}))
    (is (= (u/mgu {:type :s-var :sym 'a}
                  {:type :s-var :sym 'a})
           {})))
  (testing "function types"
    (is (= (u/mgu {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'a}]},
                   :output {:type :s-var, :sym 'a}}
                  {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'b}]},
                   :output {:type :s-var, :sym 'b}})
           {'a {:type :s-var :sym 'b}}))
    (is (= (u/mgu {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'a}
                                       {:type :s-var, :sym 'a}]},
                   :output {:type :s-var, :sym 'a}}
                  {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'b}
                                       {:type :s-var, :sym 'b}]},
                   :output {:type :s-var, :sym 'b}})
           {'a {:type :s-var :sym 'b}}))
    (is (= (u/mgu {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'a}]},
                   :output {:type :s-var, :sym 'a}}
                  {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'b}]},
                   :output {:type  :vector
                            :child {:type :s-var, :sym 'b}}})
           {:mgu-failure :occurs-check
            :schema-1    {:type :s-var, :sym 'b}
            :schema-2    {:type  :vector
                          :child {:type :s-var, :sym 'b}}})))
  (testing "map types"
    (is (= (u/mgu {:type  :map-of
                   :key   {:type 'string?}
                   :value {:type :s-var, :sym 'v}}
                  {:type  :map-of
                   :key   {:type :s-var, :sym 'k}
                   :value {:type 'boolean?}})
           {'k  {:type 'string?}
            'v {:type 'boolean?}})))
  (testing "tuple types"
    (is (= (u/mgu {:type     :tuple
                   :children [{:type :s-var, :sym 'a}
                              {:type 'int?}]}
                  {:type     :tuple
                   :children [{:type 'string?}
                              {:type :s-var, :sym 'b}]})
           {'a {:type 'string?}
            'b {:type 'int?}}))
    (is (u/mgu-failure? (u/mgu {:type     :tuple
                                :children [{:type :s-var, :sym 'a}
                                           {:type 'int?}
                                           {:type :s-var, :sym 'c}]}
                               {:type     :tuple
                                :children [{:type 'string?}
                                           {:type :s-var, :sym 'b}]})))))