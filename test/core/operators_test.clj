(ns core.relational-test)

(deftest relational-operators-test
  (let [rel (create-relation '[id name] #{[1 "Arthur"] [2 "Betty"]})] 
    (testing "Rename"
      (is (= (create-relation '[key name] #{[2 "Betty"] [1 "Arthur"]})
            (rename rel 'id 'key)) )
      (is (not= rel (rename rel 'id 'key)))
      (is (thrown? IllegalArgumentException (rename rel 'foo 'bar))))
    
    (testing "Projection"
      (is (= rel (project rel '[id name])))
      (is (= (create-relation '[name] #{["Arthur"] ["Betty"]})
            (project rel '[name])))
      (is (= (create-relation [] #{})
            (project rel [])))
      (is (thrown? IllegalArgumentException (project rel '[foo]))))))