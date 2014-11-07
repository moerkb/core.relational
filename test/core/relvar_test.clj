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
  
  #_(testing "Assignemnt with key feature"
     (let [rvar (relvar (rel {:id 1 :name "Arthur"}) {:id :key})]
       (is (= (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Bethy"}})
             (insert! rvar {:id 3 :name "Carl"})))
       (is (thrown? IllegalArgumentException (insert! rvar {:id 2 :name "Dora"})))))
  
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