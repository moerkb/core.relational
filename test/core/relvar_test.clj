(ns core.relational-test)

(deftest assign!-test
  (testing "Assignment of relation with independent constraints." 
    (let [rvar (relvar (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"}})
                [(fn [r] (every? #(< % 21) (:id r)))
                 (fn [r] (apply distinct? (map :id (seq r))))])]
     (is (= (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"} {:id 3 :name "Carl"}})
           (assign! rvar (union @rvar (rel {:id 3 :name "Carl"})))))
     (is (thrown? IllegalArgumentException (assign! rvar (union @rvar (rel {:id 1  :name "Dora"})))))
     (is (thrown? IllegalArgumentException (assign! rvar (union @rvar (rel {:id 42 :name "Dora"})))))))
  
  (testing "Assignment with unique attribute"
    (let [rvar (relvar (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Carl"}}) {:unique :id})]
      (is (thrown? IllegalArgumentException (relvar (rel {:id 1 :name "Arthur"}) {:key :id})))
      (is (= (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Carl"} {:id 3 :name "Carl"}})
            (insert! rvar {:id 3 :name "Carl"})))
      (is (thrown? IllegalArgumentException (insert! rvar {:id 2 :name "Dora"}))))
    (let [rvar (relvar (rel {:a1 1 :a2 1})
                       {:unique #{:a1 :a2}})]
      (is (= (rel #{{:a1 1 :a2 1} {:a1 1 :a2 2}})
             (insert! rvar {:a1 1 :a2 2})))))
  
  (testing "Assignment with primary keys"
    (let [rvar1 (relvar (rel {:id 1 :name "Arthur"}) {:primary-key :id})
          rvar2 (relvar (rel {:id1 1 :id2 5 :name "Arthur"}) {:primary-key #{:id1 :id2}})]
      (is (= (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Beth"}})
             (insert! rvar1 {:id 2 :name "Beth"})))
      (is (thrown? IllegalArgumentException (insert! rvar1 {:id 2 :name "Carl"})))
      (is (= (rel #{{:id1 1 :id2 5 :name "Arthur"} {:id1 1 :id2 6 :name "Beth"}})
             (insert! rvar2 {:id1 1 :id2 6 :name "Beth"})))
      (is (thrown? IllegalArgumentException (insert! rvar2 {:id1 1 :id2 6 :name "Carl"})))))
  
  (testing "Assignment with foreign keys (references)"
    (let [rperson (relvar (rel {:id 1 :name "Arthur"}))
          rphone  (relvar (rel {:id 1 :pid 1 :number "1234"})
                          {:foreign-key {:key :pid, :referenced-relvar rperson, :referenced-key :id}})]
      (is (= (rel #{{:id 1 :pid 1 :number "1234"} {:id 2 :pid 1 :number "2345"}})))
      (is (thrown? IllegalArgumentException (insert! rphone {:id 3 :pid 2 :number "3456"})))
      (is (thrown? IllegalArgumentException (delete! rperson (relfn [t]
                                                                    (= 1 (:id t))))))
      (is (thrown? IllegalArgumentException (assign! rperson (rel {:id 42 :name "Droggelbecher"}))))))
  
  (testing "Assignment of relation with constraints depending on other relations."
    (let [r (rel #{{:pid 1 :phone "123456789"}}) 
          rvar1 (relvar (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"}}))
          rvar2 (relvar r
                  (fn [r] (every? #(in? (project @rvar1 [:id]) {:id %}) (:pid r))))]
      (is (= (union r (rel {:pid 2 :phone "987654321"}))
             (assign! rvar2 (union @rvar2 (rel {:pid 2 :phone "987654321"})))))
      (is (thrown? IllegalArgumentException (assign! rvar2 (union @rvar2 
                                                             (rel {:pid 4 :phone "32132"})))))))
  
  (testing "Assignment with wrong type"
    (is (thrown? IllegalArgumentException (assign! (relvar (rel {:id 5})) dum)))))

(deftest insert!-test
  (let [rvar (relvar (rel {:id 1 :name "Arthur"})
                     [(fn [r] (every? #(> % 0) (:id r)))])
        res1 (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Bethy"}})
        res2 (union res1 (rel #{{:id 3 :name "Carl"} {:id 4 :name "Danny"}}))]
    (is (= res1 (insert! rvar {:id 2 :name "Bethy"})))
    (is (= res2 (insert! rvar #{{:id 3 :name "Carl"} {:id 4 :name "Danny"}})))
    (is (thrown? IllegalArgumentException (insert! rvar {:id -5 :name "Tony"})))
    (is (thrown? IllegalArgumentException (insert! rvar {:id 5})))))

(deftest delete!-test
  (let [rvar (relvar (rel #{{:id 1 :name "Arthur"} 
                            {:id 2 :name "Bethy"} 
                            {:id 3 :name "Carl"} 
                            {:id 4 :name "Danny"}})
               (fn [r] (every? #(> % 0) (:id r))))]
    (is (= (rel #{{:id 1 :name "Arthur"} 
                  {:id 3 :name "Carl"}})
          (delete! rvar (relfn [t] (even? (:id t))))))))

(deftest update!-test
  (let [rvar (relvar (rel #{{:id 1 :name "Arthur"} 
                            {:id 2 :name "Bethy"} 
                            {:id 3 :name "Carl"} 
                            {:id 4 :name "Danny"}})
               (relfn [r] (every? #(> % 0) (:id r))))]
    (is (= (rel [:id :name] #{[1 "John"] [2 "Bethy"] [3 "John"] [4 "Danny"]})
          (update! rvar (relfn [t] (odd? (:id t))) :name "John")))
    (is (= (rel [:id :name] #{[1 "John"] [6 "Bethy"] [3 "John"] [12 "Danny"]})
          (update! rvar (relfn [t] (even? (:id t))) :id (relfn [t] (* 3 (:id t))))))))

(deftest constraint-reset!-test
  (let [rvar (relvar (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Arthur"}})
                     {:unique :id})]
    (is (thrown? IllegalArgumentException (insert! rvar {:id 2 :name "Beth"})))
    
    (constraint-reset! rvar nil)
    (= (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Arthur"} {:id 2 :name "Beth"}})
       (insert! rvar {:id 2 :name "Beth"}))
    
    (is (thrown? IllegalArgumentException (constraint-reset! rvar {:unique :name}))) 
    (is (thrown? IllegalArgumentException (constraint-reset! rvar {:unique :id}))) 
    (constraint-reset! rvar (fn [r] (every? #(> (:id %) 0) r)))
    (= (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Arthur"} {:id 2 :name "Beth"} {:id 42 :name "Dexter"}})
       (insert! rvar {:id 42 :name "Dexter"}))
    (is (thrown? IllegalArgumentException (insert! rvar {:id 0 :name "Homer"})))))

(deftest add-constraint!-test
  (let [rvar (relvar (rel #{{:id 1 :name "Arthur"}}))]
    (is (= #rel #{{:id 1 :name "Arthur"} {:id 1 :name "Bethy"}}
          (insert! rvar {:id 1 :name "Bethy"})))
    (is (thrown? IllegalArgumentException (add-constraint! rvar {:unique :id})))
    (is (= #rel #{{:id 1 :name "Arthur"} {:id 2 :name "Bethy"}}
          (update! rvar #(= "Bethy" (:name %)) :id 2)))
    (add-constraint! rvar {:unique :id})))