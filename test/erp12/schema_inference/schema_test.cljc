(ns erp12.schema-inference.schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.schema-inference.schema :refer :all]
            [clojure.core.match :refer [match]]))

(deftest free-type-vars-test
  (is (= #{'t1} (free-type-vars '[:s-var t1])))
  (is (= #{} (free-type-vars int?)))
  (is (= #{'t1 't2} (free-type-vars '[:=> [:cat [:s-var t1]] [:s-var t2]])))
  (is (= #{'t1 't2} (free-type-vars '[:=> [:cat [:s-var t1] [:s-var t2]] [:s-var t1]])))
  (testing "scheme"
    (is (= #{'t2}
           (free-type-vars {:s-vars ['t1]
                            :body   [:=> [:cat [:s-var 't1]] [:s-var 't2]]}))))
  (testing "environments"
    (is (= #{'t1 't3}
           (free-type-vars '[[:= x [:s-var t1]]
                             [:= y {:s-vars [t2] :body [:s-var t3]}]])))))

(deftest substitute-type-test
  (is (= '[:s-var t2]
         (substitute-types '{t1 [:s-var t2]} '[:s-var t1])))
  (is (= '[:s-var s]
         (substitute-types '{t1 [:s-var t2]} '[:s-var s])))
  (is (= int? (substitute-types '{t1 [:s-var t2]} int?)))
  (testing "function types"
    (is (= '[:=> [:cat [:s-var t2]] [:s-var t2]]
           (substitute-types '{t1 [:s-var t2]}
                             '[:=> [:cat [:s-var t1]] [:s-var t1]]))))
  (testing "schemes"
    (match (substitute-types '{t1 [:s-var t2]} '{:s-vars [a] :body [:s-var t1]})
      {:s-vars [S] :with [] :body [:s-var T1]}
      (do
        (is (gen-s-var? S))
        (is (= T1 't2))))
    (match (substitute-types '{t1 [:s-var t2]} {:s-vars ['t1] :body [:s-var 't1]})
      {:s-vars [S] :body [:s-var S1]}
      (do
        (is (gen-s-var? S))
        (is (= S S1)))))
  (testing "environments"
    (let [actual (substitute-types '{t1 [:s-var t2]
                                     s1 [:s-var s2]}
                                   [[:= 'x int?]
                                    [:= 'y [:s-var 't1]]
                                    [:= 'z {:s-vars ['a] :body [:s-var 's1]}]])]
      (is (= (typings-of actual 'x) [int?]))
      (is (= (typings-of actual 'y) [[:s-var 't2]]))
      (is (= 1 (count (typings-of actual 'z))))
      (let [{:keys [s-vars with body]} (first (typings-of actual 'z))]
        (is (= 1 (count s-vars)))
        (is (gen-s-var? (first s-vars)))
        (is (= with []))
        (is (= body [:s-var 's2]))))))

(deftest compose-substitutions-test
  (is (= {'t1 string? 't2 string?}
         (compose-substitutions {'t1 string?
                                 't2 int?}
                                {'t2 [:s-var 't1]}))))

(deftest generalize-test
  (let [env [[:= 'x int?]
             [:= 'y [:s-var 't1]]]]
    (is (= int? (generalize env int?)))
    (is (= [:s-var 't1] (generalize env [:s-var 't1])))
    (is (= {:s-vars ['t2] :body [:s-var 't2]}
           (generalize env '[:s-var t2])))))

(deftest instantiate-test
  (is (= int? (instantiate int?)))
  (is (= [:s-var 't] (instantiate [:s-var 't])))
  (match (instantiate {:s-vars ['t] :body [:s-var 't]})
    [:s-var s-var]
    (is (gen-s-var? s-var)))
  (is (= int? (instantiate {:s-vars ['t] :body int?}))))

(deftest bind-type-var-test
  (is (= {'t int?} (bind-type-var 't int?)))
  (is (= {'f [:=> [:cat int?] int?]}
         (bind-type-var 'f [:=> [:cat int?] int?])))
  (is (= {} (bind-type-var 't [:s-var 't])))
  (is (thrown?
        clojure.lang.ExceptionInfo
        (bind-type-var 't [:=> [:cat [:s-var 't]] [:s-var 't]]))))

(deftest mgu-test
  (is (= {'t int?} (mgu [:s-var 't] int?)))
  (is (= {'t int?} (mgu int? [:s-var 't])))
  (is (= {} (mgu [:=> [:cat int?] string?]
                 [:=> [:cat int?] string?])))
  (is (= {'t int?}
         (mgu [:=> [:cat [:s-var 't]] string?]
              [:=> [:cat int?] string?])))
  (is (thrown?
        clojure.lang.ExceptionInfo
        (mgu [:=> [:cat [:s-var 't]] [:s-var 't]]
             [:=> [:cat int?] string?])))
  (is (= '{t [:s-var s]}
         (mgu [:=> [:cat [:s-var 't]] [:s-var 't]]
              [:=> [:cat [:s-var 's]] [:s-var 's]])))
  (is (= {'t string?}
         (mgu [:=> [:cat int?] string?]
              [:=> [:cat int?] [:s-var 't]])))
  (is (= {'a [:vector char?]}
         (mgu [:vector [:vector char?]]
              [:vector [:s-var 'a]])))
  (testing "multiple occurrences of s-var"
    (is (= {'a int? 'b string? 'c [:vector string?]}
           (mgu [:=> [:cat [:=> [:cat [:s-var 'a]]
                            [:s-var 'b]]
                      [:vector [:s-var 'a]]]
                 [:vector [:s-var 'b]]]
                [:=> [:cat [:=> [:cat int?]
                            string?]
                      [:vector int?]]
                 [:s-var 'c]])))
    (is (thrown?
          clojure.lang.ExceptionInfo
          (mgu [:=> [:cat [:=> [:cat [:s-var 'a]]
                           [:s-var 'b]]
                     [:vector [:s-var 'a]]]
                [:vector [:s-var 'a]]]
               [:=> [:cat [:=> [:cat float?]
                           string?]
                     [:vector int?]]
                [:s-var 'c]])))))

(deftest lcg-test
  (is (= int? (lcg int? int?)))

  (match (lcg int? [:vector string?])
    {:s-vars [S]
     :body   [:s-var S1]}
    (do
      (is (gen-s-var? S))
      (is (= S S1))))

  (match (lcg [:map-of int? [:vector string?]]
              [:map-of int? [:vector boolean?]])
    {:s-vars [S]
     :body   [:map-of int? [:vector [:s-var S1]]]}
    (do
      (is (gen-s-var? S))
      (is (= S S1))))

  (match (lcg [:map-of string? [:vector string?]]
              [:map-of int? [:vector boolean?]])
    {:s-vars [S T]
     :body   [:map-of [:s-var V1] [:vector [:s-var V2]]]}
    (do
      (is (gen-s-var? S))
      (is (gen-s-var? T))
      (is (not= S T))
      (is (contains? #{S T} V1))
      (is (contains? #{S T} V2))))

  (match (lcg [:map-of string? [:vector string?]]
              [:map-of boolean? [:vector boolean?]])
    {:s-vars [S]
     :body   [:map-of [:s-var S1] [:vector [:s-var S2]]]}
    (do
      (is (gen-s-var? S))
      (is (= S S1 S2))))

  ;; From "POLYMORPHIC TYPE INFERENCE FOR LANGUAGES WITH OVERLOADING AND SUBTYPING" by Geoffrey Smith
  ;; https://users.cs.fiu.edu/~smithg/papers/thesis91.pdf
  (match (lcg [:=> [:cat char? char?] boolean?]
              [:=> [:cat :number :number] boolean?]
              {:s-vars ['a]
               :with   [[:= '<= [:=> [:cat [:s-var 'a] [:s-var 'a]] boolean?]]]
               :body   [:=> [:cat [:vector [:s-var 'a]] [:vector [:s-var 'a]]] boolean?]})
    {:s-vars [S]
     :body   [:=> [:cat [:s-var S1] [:s-var S2]] boolean?]}
    (do
      (is (gen-s-var? S))
      (is (= S S1 S2)))))

(deftest overlapping?-test
  (is (not (overlapping? int? float?)))
  (is (overlapping? int? int?))
  (is (overlapping? int? {:s-vars ['a] :body [:s-var 'a]}))
  (is (not (overlapping? int? {:s-vars ['a] :body [:vector [:s-var 'a]]})))
  (is (not (overlapping?
             [[:= 'x int?]
              [:= 'y {:s-vars ['a] :body [:vector [:s-var 'a]]}]
              [:= 'z string?]])))
  (is (overlapping?
        [[:= 'x int?]
         [:= 'y {:s-vars ['a] :body [:s-var 'a]}]
         [:= 'z string?]])))

(deftest satisfiable?-test
  (is (satisfiable? [[:= 'c char?]
                     [:= 'c {:s-vars ['a]
                             :with   [[:= 'c [:s-var 'a]]]
                             :body   [:vector [:s-var 'a]]}]]
                    [[:= 'c [:vector [:vector char?]]]]))
  (is (not (satisfiable? [[:= 'c char?]
                          [:= 'c {:s-vars ['a]
                                  :with   [[:= 'c [:s-var 'a]]]
                                  :body   [:vector [:s-var 'a]]}]]
                         [[:= 'c [:vector [:vector int?]]]]))))
