(ns core.relational-test)

(deftest relational-operators-test
  (let [r (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
        product-rel (rel [:BillId :ProductId :Qty] #{[5 42 3] [5 21 7] [7 42 5]})
        wrap-start (rel [:id :name :street :zip :city]
                                        #{[1 "Arthur" "Main Street 123" 12345 "New York"]
                                          [2 "Betty" "Fake Street 321" 54321 "York"]})
        wrap-dest  (rel [:id :name :address]
                                    #{[1 "Arthur" {:street "Main Street 123"
                                                   :zip    12345
                                                   :city   "New York"}]
                                      [2 "Betty"  {:street "Fake Street 321"
                                                   :zip    54321
                                                   :city   "York"}]})] 
    (testing "Rename"
      (is (= (rel [:key :name] #{[2 "Betty"] [1 "Arthur"]})
            (rename r {:id :key})))
      (is (= (rel [:key :pre-name] #{[2 "Betty"] [1 "Arthur"]})
            (rename r {:id :key, :name :pre-name}))))
    
    (testing "Rename all"
      (is (= (rel [:pre-id :pre-name] #{[1 "Arthur"] [2 "Betty"]})
             (rename* r #"(.+)" "pre-$1"))))
    
    (testing "Restrict"
      (is (= (rel [:id :name] #{[2 "Betty"]})
             (restrict r (relfn [t] (= (:id t) 2))))))
    
    (testing "Projection with collection"
      (is (= r (project r [:id :name])))
      (is (= (rel [:name] #{["Arthur"] ["Betty"]})
            (project r [:name])))
      (is (= table-dee (project r [])))
      (is (= table-dee (project r [:foo]))))
    
    (testing "Projection with hash map"
      (is (= (rel [:pre-name] #{["Arthur"] ["Betty"]})
             (project r {:pre-name :name})))
      (is (= (rel [:new-id :pre-name] #{[2 "Arthur"] [3 "Betty"]})
             (project r {:new-id (relfn [t] (inc (:id t))), :pre-name :name}))))
    
    (testing "Project- (remove)"
      (is (= (rel [:name] #{["Arthur"] ["Betty"]})
             (project- r #{:id}))))
    
    (testing "Add-to (extend)"
      (is (= (rel [:id :name :status] 
                              #{[1 "Arthur" 20] [2 "Betty" 40]})
             (add-to r {:status (relfn [t] (* 20 (:id t)))})))
      (is (= (rel [:id :name :status]
                              #{[1 "Arthur" 50] [2 "Betty" 50]})
             (add-to r {:status 50}))))
    
    (testing "Union"
      (let [r (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
            result (rel [:id :name] #{[1 "Arthur"] [2 "Betty"] [3 "Carl"]})
            empty-rel (rel [] #{})]
        (is (= result (union r (rel [:id :name] #{[3 "Carl"]}))))
        (is (= result (union r (rel [:name :id] #{["Carl" 3]}))))
        (is (= result (union r (rel [:name :id] #{["Betty" 2] ["Carl" 3]}))))
        (is (thrown? IllegalArgumentException (union result empty-rel)))
        (is (thrown? IllegalArgumentException (union r (rel [:name] #{["Carl"]}))))))
    
    (testing "Intersect"
      (let [r (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
            result (rel [:id :name] #{[2 "Betty"]})
            empty-rel (rel [] #{})]
        (is (= result (intersect r (rel [:id :name] #{[3 "Carl"] [2 "Betty"]}))))
        (is (= result (intersect r (rel [:name :id] #{["Carl" 3] ["Betty" 2]}))))
        (is (= (rel [:id :name] #{}) (intersect r (rel [:name :id] #{["Carl" 3]}))))
        (is (thrown? IllegalArgumentException (intersect empty-rel result)))
        (is (thrown? IllegalArgumentException (union r (rel [:name] #{["Carl"]}))))))
    
    (testing "Difference"
      (is (= (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
             (difference (rel [:id :name] #{[1 "Arthur"] [2 "Betty"] [3 "Carl"]})
                         (rel {:id 3 :name "Carl"})))))
    
    (testing "Divide"
      (is (= (rel {:vater "Moritz", :mutter "Melanie"})
             (divide (rel [:vater :mutter :kind :alter]
                                      #{["Franz" "Helga" "Harald" 5]
                                        ["Franz" "Helga" "Maria" 4]
                                        ["Franz" "Ursula" "Sabine" 2]
                                        ["Moritz" "Melanie" "Gertrud" 7]
                                        ["Moritz" "Melanie" "Maria" 4]
                                        ["Moritz" "Melanie" "Sabine" 2]
                                        ["Peter" "Christina" "Robert" 9]})
                     (rel [:kind :alter]
                                      #{["Maria" 4]
                                        ["Sabine" 2]})))))
    
    (testing "Tclose"
      (let [bin-rel (rel [:from :to] #{[1 2] [2 3] [2 4] [3 5] [3 6] [4 5] [6 7]})
            tclose_bin_rel (union bin-rel (rel [:from :to]
                                            #{[1 3] [1 4] [1 5] [1 6] [1 7] [2 5] [2 6] [2 7] [3 7]}))]
        (is (= tclose_bin_rel (tclose bin-rel)))))
    
    (testing "Join, case 1: natural join"
      (is (= (rel [:id :name :phone] #{[2 "Betty" "+49 641 12345"] [1 "Arthur" "+49 2931 12345"]})
            (join r (rel [:id :phone] #{[2 "+49 641 12345"] [1 "+49 2931 12345"]}))))
      (is (= (rel [:id :phone :name] #{[2 "+49 641 12345" "Betty"] [1 "+49 2931 12345" "Arthur"]})
            (join (rel [:id :phone] #{[2 "+49 641 12345"] [1 "+49 2931 12345"]}) r))))
    
    (testing "Join, case 2: cross join"
      (is (= (rel [:id :name :key :phone] #{[1 "Arthur" "A's number" "+49 641 12345"]
                                                        [1 "Arthur" "B's number" "+49 221 12345"]
                                                        [2 "Betty" "A's number" "+49 641 12345"]
                                                        [2 "Betty" "B's number" "+49 221 12345"]})
            (join r (rel [:key :phone] #{["A's number" "+49 641 12345"] 
                                                       ["B's number" "+49 221 12345"]}))))
      (is (= (rel [:key :phone :id :name] #{["A's number" "+49 641 12345" 1 "Arthur"]
                                                        ["B's number" "+49 221 12345" 1 "Arthur"]
                                                        ["A's number" "+49 641 12345" 2 "Betty"]
                                                        ["B's number" "+49 221 12345" 2 "Betty"]})
            (join (rel [:key :phone] #{["A's number" "+49 641 12345"] 
                                                   ["B's number" "+49 221 12345"]}) r))))
    
    (testing "Join, case 3: intersect"
      (is (= (rel [:id :name] #{[2 "Betty"]})
            (join r (rel [:name :id] #{["Betty" 2] ["Carl" 3]})))))
    
    (testing "Join, case 4: semi join"
      (is (= (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
            (join r (rel [:id] #{[1] [2]})))))
    
    (testing "Composition"
      (is (= (rel [:name :phone] #{["Betty" "+49 641 12345"] ["Arthur" "+49 2931 12345"]})
             (compose r (rel [:id :phone] #{[2 "+49 641 12345"] [1 "+49 2931 12345"]})))))
    
    (testing "Group"
      (is (= (rel 
               [:BillId :Positions] 
               #{[5 (rel [:ProductId :Qty] #{[42 3] [21 7]})] 
                 [7 (rel [:ProductId :Qty] #{[42 5]})]})
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
      (is (= (rel [:ProductId :PCount] #{[42 8] [21 7]})
             (summarize product-rel #{:ProductId} {:PCount #(reduce + (:Qty %))})))
      (is (= (rel [:ProductId :PCount :MaxQty] #{[42 8 5] [21 7 7]})
             (summarize product-rel #{:ProductId} {:PCount #(reduce + (:Qty %))
                                                   :MaxQty (relfn [t] (reduce max (:Qty t)))})))
      (is (= (rel {:PCount 15})
             (summarize product-rel #{} {:PCount #(reduce + (:Qty %))})))
      (is (= (.hashCode (rel {:PCount 15, :MaxQty 7}))
             (.hashCode (summarize product-rel #{} {:PCount #(reduce + (:Qty %))
                                                   :MaxQty #(reduce max (:Qty %))})))))))