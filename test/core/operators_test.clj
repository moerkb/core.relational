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
      (is (thrown? IllegalArgumentException (project rel '[foo]))))
    
    (testing "Union"
      (let [rel (create-relation '[id name] #{[1 "Arthur"] [2 "Betty"]})
            result (create-relation '[id name] #{[1 "Arthur"] [2 "Betty"] [3 "Carl"]})]
        (is (= result (union rel (create-relation '[id name] #{[3 "Carl"]}))))
        (is (= result (union rel (create-relation '[name id] #{["Carl" 3]}))))
        (is (thrown? IllegalArgumentException (union rel (create-relation '[name] #{["Carl"]}))))))))