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
    (let [r (rel #{{:pid 1 :phone "123456789"}}) 
          rvar1 (relvar (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"}}))
          rvar2 (relvar r
                  [(fn [r] (every? #(in? (project @rvar1 [:id]) {:id %}) (:pid r)))])]
      (is (= (union r (rel {:pid 2 :phone "987654321"}))
             (assign! rvar2 (union @rvar2 (rel {:pid 2 :phone "987654321"})))))
      (is (thrown? IllegalArgumentException (assign! rvar2 (union @rvar2 (rel {:pid 4 :phone "32132"}))))))))