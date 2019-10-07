(ns decorate.core-test
  (:require
    [clojure.test :refer :all]
    [decorate.core :as decorate])
  (:import
    (java.util List)))


(def target [1 2 3])

(deftest decorate-simple-interface
  (let [x (decorate/decorator
            Iterable target
            (fn [fname f]
              (if (= fname "iterator")
                (fn [this]
                  (.iterator (reverse this)))
                f)))]
    (is (= (into [] x) [3 2 1]))))

(deftest decorate-interface-plus-object
  (let [x (decorate/decorator
            Iterable target
            decorate/unmodified
            Object target
            (fn [fname f]
              (case fname
                "toString" (constantly "Banana")
                f)))]
    (is (= (into [] x) [1 2 3]))
    (is (= (str x) "Banana"))))

(deftest decorate-more-complex-interface-with-overlap
  (let [diverted-items (volatile! [])
        x (decorate/decorator
            List target
            (fn [fname f]
              (if (= fname "add")
                (fn
                  ([_this item]
                   (vswap! diverted-items conj item)
                   false)
                  ([_this _idx item]
                   (vswap! diverted-items conj item)
                   false))
                f))
            Object target
            decorate/unmodified)]

    (is (= x [1 2 3]))
    (is (= (str x) "[1 2 3]"))

    (.add x 4)
    (.add x 0 5)

    (is (= x [1 2 3]))
    (is (= @diverted-items [4 5]))))

(deftest decorate-prefers-first-method-by-signature
  (let [true-first (decorate/decorator
                     List target
                     (fn [fname f]
                       (if (= fname "equals")
                         (constantly true)
                         f))
                     Object target
                     (fn [fname f]
                       (if (= fname "equals")
                         (constantly false)
                         f)))
        false-first (decorate/decorator
                      Object target
                      (fn [fname f]
                        (if (= fname "equals")
                          (constantly false)
                          f))
                      List target
                      (fn [fname f]
                        (if (= fname "equals")
                          (constantly true)
                          f)))]
    (is (= true-first :aubergine-sandwich))
    (is (not= false-first :aubergine-sandwich))))
