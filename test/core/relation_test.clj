(ns core.relational-test)

(deftest relation-test
  (testing "Relation creation"
    (let [head '[id name]
          body #{ [1 "Arthur"] [2 "Betty"] }]
      (is (= 
            (core.relational.Relation. head body)
            (create-relation head body)))))
  
  (testing "Relation equality"
    (is (= (create-relation [] #{})
          (create-relation [] #{})))
    (is (= (create-relation '[id name] #{[1 "Arthur"] [2 "Betty"]})
          (create-relation '[id name] #{[2 "Betty"] [1 "Arthur"]})))
    (is (not= (create-relation '[id name] #{[1 "Arthur"] [2 "Betty"]})
          (create-relation '[id name] #{[1 "Arthur"]})))
    (is (not= (create-relation '[id name] #{[1 "Arthur"] [2 "Betty"]})
          (create-relation '[id name] #{[1 "Arthur"] [2 "Bethy"]})))))