(ns core.relational-test)

(deftest relational-operators-test
  (let [rel (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]})] 
    (testing "Rename"
      (is (= (create-relation [:key :name] #{[2 "Betty"] [1 "Arthur"]})
            (rename rel {:id :key})))
      (is (= (create-relation [:key :pre-name] #{[2 "Betty"] [1 "Arthur"]})
            (rename rel {:id :key, :name :pre-name}))))
    
    (testing "Projection"
      (is (= rel (project rel [:id :name])))
      (is (= (create-relation [:name] #{["Arthur"] ["Betty"]})
            (project rel [:name])))
      (is (= (create-relation [] #{})
            (project rel [])))
      (is (thrown? IllegalArgumentException (project rel [:foo]))))
    
    (testing "Union"
      (let [rel (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]})
            result (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"] [3 "Carl"]})
            empty-rel (create-relation [] #{})]
        (is (= result (union rel (create-relation [:id :name] #{[3 "Carl"]}))))
        (is (= result (union rel (create-relation [:name :id] #{["Carl" 3]}))))
        (is (= result (union rel (create-relation [:name :id] #{["Betty" 2] ["Carl" 3]}))))
        (is (thrown? IllegalArgumentException (union result empty-rel)))
        (is (thrown? IllegalArgumentException (union rel (create-relation [:name] #{["Carl"]}))))))
    
    (testing "Intersect"
      (let [rel (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]})
            result (create-relation [:id :name] #{[2 "Betty"]})
            empty-rel (create-relation [] #{})]
        (is (= result (intersect rel (create-relation [:id :name] #{[3 "Carl"] [2 "Betty"]}))))
        (is (= result (intersect rel (create-relation [:name :id] #{["Carl" 3] ["Betty" 2]}))))
        (is (= (create-relation [:id :name] #{}) (intersect rel (create-relation [:name :id] #{["Carl" 3]}))))
        (is (thrown? IllegalArgumentException (intersect empty-rel result)))
        (is (thrown? IllegalArgumentException (union rel (create-relation [:name] #{["Carl"]}))))))
    
    (testing "Join, case 1: natural join"
      (is (= (create-relation [:id :name :phone] #{[2 "Betty" "+49 641 12345"] [1 "Arthur" "+49 2931 12345"]})
            (join rel (create-relation [:id :phone] #{[2 "+49 641 12345"] [1 "+49 2931 12345"]}))))
      (is (= (create-relation [:id :phone :name] #{[2 "+49 641 12345" "Betty"] [1 "+49 2931 12345" "Arthur"]})
            (join (create-relation [:id :phone] #{[2 "+49 641 12345"] [1 "+49 2931 12345"]}) rel))))
    
    (testing "Join, case 2: cross join"
      (is (= (create-relation [:id :name :key :phone] #{[1 "Arthur" "A's number" "+49 641 12345"]
                                                        [1 "Arthur" "B's number" "+49 221 12345"]
                                                        [2 "Betty" "A's number" "+49 641 12345"]
                                                        [2 "Betty" "B's number" "+49 221 12345"]})
            (join rel (create-relation [:key :phone] #{["A's number" "+49 641 12345"] 
                                                       ["B's number" "+49 221 12345"]}))))
      (is (= (create-relation [:key :phone :id :name] #{["A's number" "+49 641 12345" 1 "Arthur"]
                                                        ["B's number" "+49 221 12345" 1 "Arthur"]
                                                        ["A's number" "+49 641 12345" 2 "Betty"]
                                                        ["B's number" "+49 221 12345" 2 "Betty"]})
            (join (create-relation [:key :phone] #{["A's number" "+49 641 12345"] 
                                                   ["B's number" "+49 221 12345"]}) rel))))
    
    (testing "Join, case 3: intersect"
      (is (= (create-relation [:id :name] #{[2 "Betty"]})
            (join rel (create-relation [:name :id] #{["Betty" 2] ["Carl" 3]})))))
    
    (testing "Join, case 4: semi join"
      (is (= (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]})
            (join rel (create-relation [:id] #{[1] [2]})))))
    
    (testing "Group"
      (is (= (create-relation 
               [:BillId :Positions] 
               #{[5 (create-relation [:ProductId :Qty] #{[42 3] [21 7]})] 
                 [7 (create-relation [:ProductId :Qty] #{[42 5]})]})
            (group 
              (create-relation [:BillId :ProductId :Qty] #{[5 42 3] [5 21 7] [7 42 5]})
              [:ProductId :Qty]
              :Positions))))))