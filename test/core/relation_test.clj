(ns core.relational-test)

(deftest new-relation-test
  (is (= (create-relation '[id name] #{[1 "Arthur"] [2 "Betty"]})
         (new-relation #{ {'id 1 'name "Arthur"} {'id 2 'name "Betty"} }))))

(deftest rel-to-hash-map-test
  (is (= #{ {'id 1 'name "Arthur"} {'id 2 'name "Betty"} })
         (rel-to-hash-map (create-relation '[id name] #{[1 "Arthur"] [2 "Betty"]}))))

(deftest relation-test
  (testing "Relation creation"
    (let [head '[id name]
          body #{ [1 "Arthur"] [2 "Betty"] }]
      (is (= (core.relational.Relation. head body)
            (create-relation head body)))
      (is (= (core.relational.Relation. [] #{})
            (create-relation [] #{})))))
  
  (testing "Relation head integrity"
    (is (thrown? AssertionError (create-relation nil #{})))
    (is (thrown? AssertionError (create-relation '(id name) #{})))
    (is (thrown? AssertionError (create-relation '[id :name phone] #{})))
    (is (thrown? AssertionError (create-relation '[id nil phone] #{}))))
  
  (testing "Relation body integrity"
    (is (thrown? AssertionError (create-relation '[id name] nil)))
    (is (thrown? AssertionError (create-relation '[id name] [])))
    (is (thrown? AssertionError (create-relation '[id name] #{nil})))
    (is (thrown? AssertionError (create-relation '[id name] #{'(1 "Arthur")})))
    (is (thrown? AssertionError (create-relation '[id name] #{[1 nil]}))))
  
  (testing "Relation equality"
    (is (= (create-relation [] #{})
          (create-relation [] #{})))
    (is (= (create-relation '[id name] #{[1 "Arthur"] [2 "Betty"]})
          (create-relation '[id name] #{[2 "Betty"] [1 "Arthur"]})))
    (is (not= (create-relation '[id name] #{[1 "Arthur"] [2 "Betty"]})
          (create-relation '[id name] #{[1 "Arthur"]})))
    (is (not= (create-relation '[id name] #{[1 "Arthur"] [2 "Betty"]})
          (create-relation '[id name] #{[1 "Arthur"] [2 "Bethy"]})))))