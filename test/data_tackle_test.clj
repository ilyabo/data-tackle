(ns data-tackle-test
  (:require [clojure.test :refer :all]
            [data-tackle :refer :all]))

(deftest words-test
  (testing "Function words"
    (is (= ["Hello" "world"] (words "Hello, world!")))))


(deftest parse-csv-test
  (testing "Parsing csv"
    (let [csv "A;B;C\n0.80;0.38;0.24\n2.80;0.29;0.21"
          parsed  (read-csv-input csv :separator \;)]
      (is (= "0.38" (:B (first parsed))))
      (is (= "0.29" (:B (second parsed))))
      (is (= "0.21" (:C (second parsed))))
    )))
