(ns core.relational-test)

(deftest relvar-test
  (let [r (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"}})]
    (is (= r @(relvar r)))
    (is (= r @(relvar r [(fn [r] (< (:id r) 21))])))))

(deftest assign-test
  (testing "Assignment of relation with independent constraints." 
    (let [rvar (relvar (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"}})
                [(fn [r] (every? #(< % 21) (:id r)))
                 (fn [r] (apply distinct? (map :id (seq r))))])]
     (is (= (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"} {:id 3 :name "Carl"}})
           (assign! rvar (union @rvar (rel {:id 3 :name "Carl"})))))
     (is (thrown? IllegalArgumentException (assign! rvar (union @rvar (rel {:id 1  :name "Dora"})))))
     (is (thrown? IllegalArgumentException (assign! rvar (union @rvar (rel {:id 42 :name "Dora"})))))))
  
  (testing "Assignment of relation with constraints depending on other relations."
    (let [r (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"}})
          rvar1 (relvar (rel #{{:pid 1 :phone "123456789"}
                               {:pid 2 :phone "987654321"}
                               {:pid 3 :phone "112233445"}}))
          rvar2 (relvar r
                        [(fn [r] (every? #(in? (project @rvar1 [:pid]) {:pid %}) (:id r)))])]
      (is (= (union r (rel {:id 3 :name "Carl"}))
             (assign! rvar2 (union @rvar2 (rel {:id 3 :name "Carl"})))))
      (is (thrown? IllegalArgumentException (assign! rvar2 (union @rvar2 (rel {:id 4 :name "Dori"}))))))))