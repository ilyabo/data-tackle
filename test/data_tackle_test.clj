(ns data-tackle-test
  (:require [clojure.test :refer :all]
            [data-tackle :refer :all]))

(deftest words-test
  (testing "Function words"
    (is (= ["Hello" "world"] (words "Hello, world!")))))
