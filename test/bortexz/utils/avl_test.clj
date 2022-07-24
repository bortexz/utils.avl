(ns bortexz.utils.avl-test
  (:require [clojure.test :refer [deftest testing is]]
            [bortexz.utils.avl :as u-avl]
            [clojure.data.avl :as avl]))

(defn descending-comparator [a b] (compare b a))

(def sm (avl/sorted-map 1 2 3 4 5 6))
(def ss (avl/sorted-set 1 2 3))
(def ism (avl/sorted-map-by descending-comparator 1 2 3 4 5 6))
(def iss (avl/sorted-set-by descending-comparator 1 2 3))

(deftest index-by-test
  (testing "index-by works with base compare by default"
    (is (= sm (u-avl/index-by dec [2 4 6]))))
  (testing "index-by works with custom comparator"
    (is (= ism (u-avl/index-by descending-comparator dec [2 4 6])))))

(deftest nth-test
  (testing "nth works the same as clojure.core nth for existing indexes"
    (is (= (clojure.core/nth ss 1) (u-avl/nth ss 1)))
    (is (= (clojure.core/nth sm 1) (u-avl/nth sm 1))))

  (testing "nth returns nil if index does not exist"
    (is (nil? (u-avl/nth ss 3)))
    (is (nil? (u-avl/nth sm 3))))

  (testing "nth-key and nth-val"
    (is (= 1 (u-avl/nth-key sm 0)))
    (is (= 2 (u-avl/nth-val sm 0)))
    (is (nil? (u-avl/nth-key sm 4)))
    (is (nil? (u-avl/nth-val sm 4)))))

(deftest first-test
  (testing "first works the same as clojure.core nth for existing indexes"
    (is (= (clojure.core/first ss) (u-avl/first ss)))
    (is (= (clojure.core/first sm) (u-avl/first sm))))

  (testing "first returns nil if index does not exist"
    (is (nil? (u-avl/first (avl/sorted-map))))
    (is (nil? (u-avl/first (avl/sorted-set)))))

  (testing "first-key and first-val"
    (is (= 1 (u-avl/first-key sm)))
    (is (= 2 (u-avl/first-val sm)))
    (is (nil? (u-avl/first-key (avl/sorted-map))))
    (is (nil? (u-avl/first-val (avl/sorted-map))))))

(deftest last-test
  (testing "last works the same as clojure.core nth for existing indexes"
    (is (= (clojure.core/last ss) (u-avl/last ss)))
    (is (= (clojure.core/last sm) (u-avl/last sm))))

  (testing "last returns nil if index does not exist"
    (is (nil? (u-avl/last (avl/sorted-map))))
    (is (nil? (u-avl/last (avl/sorted-set)))))

  (testing "last-key and last-val"
    (is (= 5 (u-avl/last-key sm)))
    (is (= 6 (u-avl/last-val sm)))
    (is (nil? (u-avl/last-key (avl/sorted-map))))
    (is (nil? (u-avl/last-val (avl/sorted-map))))))

(deftest rank-of-test
  (testing "behaves as avl/rank-of for existing values"
    (is (= (avl/rank-of sm 1) (u-avl/rank-of sm 1)))
    (is (= (avl/rank-of ss 1) (u-avl/rank-of ss 1))))
  (testing "returns nil for non-existing indexes"
    (is (nil? (u-avl/rank-of sm 2)))
    (is (nil? (u-avl/rank-of ss 4)))))

(deftest nearest-key-val-test
  (testing "nearest-key and nearest-val"
    (is (= (key (avl/nearest sm < 3)) (u-avl/nearest-key sm < 3)))
    (is (= (val (avl/nearest sm < 3)) (u-avl/nearest-val sm < 3)))
    (is (nil? (u-avl/nearest-key sm > 5)))
    (is (nil? (u-avl/nearest-val sm > 5)))))

(deftest nearest-rank-test
  (testing "nearest-rank works"
    (is (= 0 (u-avl/nearest-rank sm < 3)))
    (is (= 0 (u-avl/nearest-rank sm < 2)))))

(deftest offset-test
  (testing "positive offset goes forward"
    (is (= [5 6] (u-avl/offset sm 3 1)))
    (is (= 3 (u-avl/offset ss 2 1))))
  
  (testing "negative offset goes backwards"
    (is (= [1 2] (u-avl/offset sm 3 -1)))
    (is (= 1 (u-avl/offset ss 2 -1))))
  
  (testing "no offset returns"
    (is (= [3 4] (u-avl/offset sm 3 0)))
    (is (= 2 (u-avl/offset ss 2 0))))
  
  (testing "out of bounds returns nil"
    (is (nil? (u-avl/offset sm 3 2)))
    (is (nil? (u-avl/offset ss 2 2))))
  
  (testing "if specified key does not exist, returns nil"
    (is (nil? (u-avl/offset sm 2 0)))
    (is (nil? (u-avl/offset ss 4 0))))
  
  (testing "offset-key and offset-val"
    (is (= (key (u-avl/offset sm 1 0)) (u-avl/offset-key sm 1 0)))
    (is (= (val (u-avl/offset sm 1 0)) (u-avl/offset-val sm 1 0)))
    (is (nil?  (u-avl/offset-key sm 1 3))) ; out-of-bounds
    (is (nil?  (u-avl/offset-val sm 4 0))) ; key-not-exists
    ))

(deftest tail-test
  (testing "base tail test"
    (is (= [[3 4] [5 6]] (u-avl/tail sm 2)))
    (is (= [2 3] (u-avl/tail ss 2))))
  
  (testing "specifying an end-key works"
    (is (= [[1 2] [3 4]] (u-avl/tail sm 2 3)))
    (is (= [1 2] (u-avl/tail ss 2 2))))
  
  (testing "returns incomplete tail if tail smaller than n"
    (is (= [[1 2] [3 4]] (u-avl/tail sm 4 3)))
    (is (= [1 2] (u-avl/tail ss 4 2))))
  
  (testing "returns nil if end-key does not exist"
    (is (nil? (u-avl/tail sm 2 4)))
    (is (nil? (u-avl/tail ss 2 4))))
  
  (testing "tail-keys and tail-vals"
    (is (= [4 6] (u-avl/tail-vals sm 2)))
    (is (= [3 5] (u-avl/tail-keys sm 2))))
  
  (testing "tail-sorted"
    (is (= (avl/sorted-map 3 4 5 6) (u-avl/tail-sorted sm 2)))))

(deftest full-tail-test
  (testing "base full-tail test"
    (is (= [[3 4] [5 6]] (u-avl/full-tail sm 2)))
    (is (nil? (u-avl/full-tail (avl/sorted-map 1 nil 3 4) 2)))) 
  
  (testing "specifying an end-key works"
    (is (= [[1 2] [3 4]] (u-avl/full-tail sm 2 3))))
  
  (testing "returns nil if end-key does not exist"
    (is (nil? (u-avl/full-tail sm 2 4))))

  (testing "returns nil if full-tail would be incomplete"
    (is (nil? (u-avl/full-tail sm 4 3)))
    (is (nil? (u-avl/full-tail ss 4 2))))
  
  (testing "full-tail-keys and full-tail-vals"
    (is (= [4 6] (u-avl/full-tail-vals sm 2))))
  
  (testing "full-tail-sorted"
    (is (= (avl/sorted-map 3 4 5 6) (u-avl/full-tail-sorted sm 2)))
    (is (nil? (u-avl/full-tail-sorted (avl/sorted-map 1 2 3 4) 3)))))