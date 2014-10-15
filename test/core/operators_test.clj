(ns core.relational-test)

(deftest relational-operators-test
  (let [rel (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
        product-rel (newrel [:BillId :ProductId :Qty] #{[5 42 3] [5 21 7] [7 42 5]})
        wrap-start (newrel [:id :name :street :zip :city]
                                        #{[1 "Arthur" "Main Street 123" 12345 "New York"]
                                          [2 "Betty" "Fake Street 321" 54321 "York"]})
        wrap-dest  (newrel [:id :name :address]
                                    #{[1 "Arthur" {:street "Main Street 123"
                                                   :zip    12345
                                                   :city   "New York"}]
                                      [2 "Betty"  {:street "Fake Street 321"
                                                   :zip    54321
                                                   :city   "York"}]})] 
    (testing "Rename"
      (is (= (newrel [:key :name] #{[2 "Betty"] [1 "Arthur"]})
            (rename rel {:id :key})))
      (is (= (newrel [:key :pre-name] #{[2 "Betty"] [1 "Arthur"]})
            (rename rel {:id :key, :name :pre-name}))))
    
    (testing "Rename all"
      (is (= (newrel [:pre-id :pre-name] #{[1 "Arthur"] [2 "Betty"]})
             (rename* rel #"(.+)" "pre-$1"))))
    
    (testing "Restrict"
      (is (= (newrel [:id :name] #{[2 "Betty"]})
             (restrict rel (fn [t] (= (:id t) 2))))))
    
    (testing "Projection with collection"
      (is (= rel (project rel [:id :name])))
      (is (= (newrel [:name] #{["Arthur"] ["Betty"]})
            (project rel [:name])))
      (is (= table-dee (project rel [])))
      (is (= table-dee (project rel [:foo]))))
    
    (testing "Projection with hash map"
      (is (= (newrel [:pre-name] #{["Arthur"] ["Betty"]})
             (project rel {:pre-name :name})))
      (is (= (newrel [:new-id :pre-name] #{[2 "Arthur"] [3 "Betty"]})
             (project rel {:new-id (fn [t] (inc (:id t))), :pre-name :name}))))
    
    (testing "Project- (remove)"
      (is (= (newrel [:name] #{["Arthur"] ["Betty"]})
             (project- rel #{:id}))))
    
    (testing "Add-to (extend)"
      (is (= (newrel [:id :name :status] 
                              #{[1 "Arthur" 20] [2 "Betty" 40]})
             (add-to rel {:status (fn [t] (* 20 (:id t)))})))
      (is (= (newrel [:id :name :status]
                              #{[1 "Arthur" 50] [2 "Betty" 50]})
             (add-to rel {:status 50}))))
    
    (testing "Union"
      (let [rel (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
            result (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"] [3 "Carl"]})
            empty-rel (newrel [] #{})]
        (is (= result (union rel (newrel [:id :name] #{[3 "Carl"]}))))
        (is (= result (union rel (newrel [:name :id] #{["Carl" 3]}))))
        (is (= result (union rel (newrel [:name :id] #{["Betty" 2] ["Carl" 3]}))))
        (is (thrown? IllegalArgumentException (union result empty-rel)))
        (is (thrown? IllegalArgumentException (union rel (newrel [:name] #{["Carl"]}))))))
    
    (testing "Intersect"
      (let [rel (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
            result (newrel [:id :name] #{[2 "Betty"]})
            empty-rel (newrel [] #{})]
        (is (= result (intersect rel (newrel [:id :name] #{[3 "Carl"] [2 "Betty"]}))))
        (is (= result (intersect rel (newrel [:name :id] #{["Carl" 3] ["Betty" 2]}))))
        (is (= (newrel [:id :name] #{}) (intersect rel (newrel [:name :id] #{["Carl" 3]}))))
        (is (thrown? IllegalArgumentException (intersect empty-rel result)))
        (is (thrown? IllegalArgumentException (union rel (newrel [:name] #{["Carl"]}))))))
    
    (testing "Difference"
      (is (= (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
             (difference (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"] [3 "Carl"]})
                         (newrel {:id 3 :name "Carl"})))))
    
    (testing "Divide"
      (is (= (newrel {:vater "Moritz", :mutter "Melanie"})
             (divide (newrel [:vater :mutter :kind :alter]
                                      #{["Franz" "Helga" "Harald" 5]
                                        ["Franz" "Helga" "Maria" 4]
                                        ["Franz" "Ursula" "Sabine" 2]
                                        ["Moritz" "Melanie" "Gertrud" 7]
                                        ["Moritz" "Melanie" "Maria" 4]
                                        ["Moritz" "Melanie" "Sabine" 2]
                                        ["Peter" "Christina" "Robert" 9]})
                     (newrel [:kind :alter]
                                      #{["Maria" 4]
                                        ["Sabine" 2]})))))
    
    (testing "Tclose"
      (let [bin-rel (newrel [:from :to] #{[1 2] [2 3] [2 4] [3 5] [3 6] [4 5] [6 7]})
            tclose_bin_rel (union bin-rel (newrel [:from :to]
                                            #{[1 3] [1 4] [1 5] [1 6] [1 7] [2 5] [2 6] [2 7] [3 7]}))]
        (is (= tclose_bin_rel (tclose bin-rel)))))
    
    (testing "Join, case 1: natural join"
      (is (= (newrel [:id :name :phone] #{[2 "Betty" "+49 641 12345"] [1 "Arthur" "+49 2931 12345"]})
            (join rel (newrel [:id :phone] #{[2 "+49 641 12345"] [1 "+49 2931 12345"]}))))
      (is (= (newrel [:id :phone :name] #{[2 "+49 641 12345" "Betty"] [1 "+49 2931 12345" "Arthur"]})
            (join (newrel [:id :phone] #{[2 "+49 641 12345"] [1 "+49 2931 12345"]}) rel))))
    
    (testing "Join, case 2: cross join"
      (is (= (newrel [:id :name :key :phone] #{[1 "Arthur" "A's number" "+49 641 12345"]
                                                        [1 "Arthur" "B's number" "+49 221 12345"]
                                                        [2 "Betty" "A's number" "+49 641 12345"]
                                                        [2 "Betty" "B's number" "+49 221 12345"]})
            (join rel (newrel [:key :phone] #{["A's number" "+49 641 12345"] 
                                                       ["B's number" "+49 221 12345"]}))))
      (is (= (newrel [:key :phone :id :name] #{["A's number" "+49 641 12345" 1 "Arthur"]
                                                        ["B's number" "+49 221 12345" 1 "Arthur"]
                                                        ["A's number" "+49 641 12345" 2 "Betty"]
                                                        ["B's number" "+49 221 12345" 2 "Betty"]})
            (join (newrel [:key :phone] #{["A's number" "+49 641 12345"] 
                                                   ["B's number" "+49 221 12345"]}) rel))))
    
    (testing "Join, case 3: intersect"
      (is (= (newrel [:id :name] #{[2 "Betty"]})
            (join rel (newrel [:name :id] #{["Betty" 2] ["Carl" 3]})))))
    
    (testing "Join, case 4: semi join"
      (is (= (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
            (join rel (newrel [:id] #{[1] [2]})))))
    
    (testing "Composition"
      (is (= (newrel [:name :phone] #{["Betty" "+49 641 12345"] ["Arthur" "+49 2931 12345"]})
             (compose rel (newrel [:id :phone] #{[2 "+49 641 12345"] [1 "+49 2931 12345"]})))))
    
    (testing "Group"
      (is (= (newrel 
               [:BillId :Positions] 
               #{[5 (newrel [:ProductId :Qty] #{[42 3] [21 7]})] 
                 [7 (newrel [:ProductId :Qty] #{[42 5]})]})
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
      (is (= (newrel [:ProductId :PCount] #{[42 8] [21 7]})
             (summarize product-rel #{:ProductId} {:PCount #(reduce + (:Qty %))})))
      (is (= (newrel [:ProductId :PCount :MaxQty] #{[42 8 5] [21 7 7]})
             (summarize product-rel #{:ProductId} {:PCount #(reduce + (:Qty %))
                                                   :MaxQty #(reduce max (:Qty %))})))
      (is (= (newrel {:PCount 15})
             (summarize product-rel #{} {:PCount #(reduce + (:Qty %))})))
      (is (= (.hashCode (newrel {:PCount 15, :MaxQty 7}))
             (.hashCode (summarize product-rel #{} {:PCount #(reduce + (:Qty %))
                                                   :MaxQty #(reduce max (:Qty %))})))))))