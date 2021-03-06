(ns dt-test
  (:require [clojure.test :refer :all]
            [dt :refer :all]))

(deftest words-test
  (testing "Function words"
    (is (= ["Hello" "world"] (words "Hello, world!")))))


(deftest parse-csv-test
  (testing "Parsing csv with ; as separator"
    (let [csv "A;B;C\n0.80;0.38;0.24\n2.80;0.29;0.21"
          parsed  (read-csv-input csv :separator \;  :use-keywords false)]
      (is (= "0.38" (get (first parsed) "B")))
      (is (= "0.29" (get (second parsed) "B")))
      (is (= "0.21" (get (second parsed) "C")))
    )))


(deftest parse-csv-from-lines
  (testing "Parsing csv composed from lines"
    (let [lines   ["A;B;C" "0.80;0.38;0.24" "0.80;0.29;0.21"]
          csv     (clojure.string/join "\n" lines)
          parsed  (read-csv-input csv :separator \;  :use-keywords false)]
      (is (= "0.38" (get (first parsed) "B")))
      (is (= "0.29" (get (second parsed) "B")))
      (is (= "0.21" (get (second parsed) "C")))
    )))



(deftest parse-csv-test-keywords
  (testing "Parsing csv with ; as separator"
    (let [csv "A;B;C\n0.80;0.38;0.24\n2.80;0.29;0.21"
          parsed  (read-csv-input csv :separator \;)]
      (is (= "0.38" (:B (first parsed))))
      (is (= "0.29" (:B (second parsed))))
      (is (= "0.21" (:C (second parsed))))
    )))



(deftest parse-csv-multiple-rows-header-test
  (testing "Parsing csv with two header rows"
    (let [csv "A,B,C\n1,2,3\n0.80,0.38,0.24\n2.80,0.29,0.21"
          parsed  (read-csv-input csv :header-rows 2  :use-keywords false)]
      (is (= "0.38" (get (first parsed) ["B" "2"])))
      (is (= "0.29" (get (second parsed) ["B" "2"])))
      (is (= "0.21" (get (second parsed) ["C" "3"])))
    )))



(deftest parse-csv-multiple-rows-header-test-keywords
  (testing "Parsing csv with two header rows"
    (let [csv "A,B,C\n1,2,3\n0.80,0.38,0.24\n2.80,0.29,0.21"
          parsed  (read-csv-input csv :header-rows 2)]
      (is (= "0.38" (get (first parsed) [:B :2])))
      (is (= "0.29" (get (second parsed) [:B :2])))
      (is (= "0.21" (get (second parsed) [:C :3])))
    )))


