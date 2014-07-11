(ns core.relational-test)

(deftest relational-operators-test
  (let [rel (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]})
        product-rel (create-relation [:BillId :ProductId :Qty] #{[5 42 3] [5 21 7] [7 42 5]})
        wrap-start (create-relation [:id :name :street :zip :city]
                                        #{[1 "Arthur" "Main Street 123" 12345 "New York"]
                                          [2 "Betty" "Fake Street 321" 54321 "York"]})
        wrap-dest  (create-relation [:id :name :address]
                                    #{[1 "Arthur" {:street "Main Street 123"
                                                   :zip    12345
                                                   :city   "New York"}]
                                      [2 "Betty"  {:street "Fake Street 321"
                                                   :zip    54321
                                                   :city   "York"}]})] 
    (testing "Rename"
      (is (= (create-relation [:key :name] #{[2 "Betty"] [1 "Arthur"]})
            (rename rel {:id :key})))
      (is (= (create-relation [:key :pre-name] #{[2 "Betty"] [1 "Arthur"]})
            (rename rel {:id :key, :name :pre-name}))))
    
    (testing "Rename all"
      (is (= (create-relation [:pre-id :pre-name] #{[1 "Arthur"] [2 "Betty"]})
             (rename* rel #"(.+)" "pre-$1"))))
    
    (testing "Restrict"
      (is (= (create-relation [:id :name] #{[2 "Betty"]})
             (restrict rel '(= :id 2)))))
    
    (testing "Projection with collection"
      (is (= rel (project rel [:id :name])))
      (is (= (create-relation [:name] #{["Arthur"] ["Betty"]})
            (project rel [:name])))
      (is (= (create-relation [] #{})
            (project rel [])))
      (is (= (new-relation {}) (project rel [:foo])))
      (is (= (new-relation nil) (project rel [:foo]))))
    
    (testing "Projection with hash map"
      (is (= (create-relation [:pre-name] #{["Arthur"] ["Betty"]})
             (project rel {:pre-name :name})))
      (is (= (create-relation [:new-id :pre-name] #{[2 "Arthur"] [3 "Betty"]})
             (project rel {:new-id '(inc :id), :pre-name :name}))))
    
    (testing "Project- (remove)"
      (is (= (create-relation [:name] #{["Arthur"] ["Betty"]})
             (project- rel #{:id}))))
    
    (testing "Add (extend)"
      (is (= (create-relation [:id :name :status] 
                              #{[1 "Arthur" 20] [2 "Betty" 40]})
             (add rel {:status '(* 20 :id)})))
      (is (= (create-relation [:id :name :status]
                              #{[1 "Arthur" 50] [2 "Betty" 50]})
             (add rel {:status 50}))))
    
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
    
    (testing "Composition"
      (is (= (create-relation [:name :phone] #{["Betty" "+49 641 12345"] ["Arthur" "+49 2931 12345"]})
             (compose rel (create-relation [:id :phone] #{[2 "+49 641 12345"] [1 "+49 2931 12345"]})))))
    
    #_(testing "Group"
       (is (= (create-relation 
                [:BillId :Positions] 
                #{[5 (create-relation [:ProductId :Qty] #{[42 3] [21 7]})] 
                  [7 (create-relation [:ProductId :Qty] #{[42 5]})]})
              (group 
                product-rel
                {:Positions #{:ProductId :Qty}}))))
    
    (testing "Ungroup"
      (is (= product-rel
             (ungroup (group product-rel {:Positions #{:ProductId :Qty}}) #{:Positions}))))
    
    (testing "Wrap"
      (is (= wrap-dest (wrap wrap-start {:address #{:street :zip :city}}))))
    
    (testing "Unwrap"
      (is (= wrap-start (unwrap (wrap wrap-start {:address #{:street :zip :city}})
                                #{:address})))
      (is (= wrap-start (unwrap (wrap wrap-start {:address #{:street :zip :city}
                                                  :main    #{:id :name}})
                                #{:address :main}))))
    
    (testing "Summarize"
      (is (= (create-relation [:ProductId :PCount] #{[42 8] [21 7]})
             (summarize product-rel #{:ProductId} {:PCount #(reduce + (:Qty %))})))
      (is (= (create-relation [:ProductId :PCount :MaxQty] #{[42 8 5] [21 7 7]})
             (summarize product-rel #{:ProductId} {:PCount #(reduce + (:Qty %))
                                                   :MaxQty #(reduce max (:Qty %))})))
      (is (= (new-relation {:PCount 15})
             (summarize product-rel #{} {:PCount #(reduce + (:Qty %))})))
      (is (= (new-relation {:PCount 15, :MaxQty 7})
             (summarize product-rel #{} {:PCount #(reduce + (:Qty %))
                                         :MaxQty #(reduce max (:Qty %))}))))))