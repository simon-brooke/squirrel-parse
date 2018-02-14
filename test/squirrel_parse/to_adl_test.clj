(ns squirrel-parse.to-adl-test
  (:require [clojure.test :refer :all]
            [squirrel-parse.to-adl :refer :all]))

(deftest test-get-column-datatype
  (testing "Testing whether right datatype is returned for a valid column"
    (let [column-spec '(:COLUMN-SPEC
                         (:NAME "longitude")
                         (:DATATYPE (:DT-REAL "real"))
                         (:COLUMN-CONSTRAINTS)
                         (:OPT-COMMA))
          actual (get-column-datatype column-spec)
          expected :real]

      (is (= expected actual)))))
