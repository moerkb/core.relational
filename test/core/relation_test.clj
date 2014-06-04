(ns core.relational-test)

(deftest relation-test
  (testing "Relation creation"
    (let [head '[id name]
          body #{ [1 "Arthur"] [2 "Betty"] }]
      (is (= 
            (core.relational.Relation. head body)
            (create-relation head body))))))