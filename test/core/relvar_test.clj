(ns core.relational-test)

(deftest involved-relvars-test
  (let [empty-var (relvar dum)
        evar2 (relvar dee)
        constrs [(relfn [t] (> (:status t) 5))
                 (relfn [t] (in? @empty-var t))
                 (relfn [t] (> (:status t) (max (:status @evar2))))]]
    (is (= #{} (involved-relvars [])))
    (is (= #{'empty-var 'evar2} (involved-relvars constrs)))))

(deftest relvar-test
  (let [r (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"}})]
    (is (= r @(relvar r)))
    (is (= r @(relvar r [(relfn [r] (every? #(< % 21) (:id r)))])))
    (is (thrown? IllegalArgumentException (relvar (rel {:id -5}) 
                                           [(relfn [r] (every? #(> % 0) (:id r)))])))))

(deftest assign-test
  (testing "Assignment of relation with independent constraints." 
    (let [rvar (relvar (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"}})
                [(relfn [r] (every? #(< % 21) (:id r)))
                 (relfn [r] (apply distinct? (map :id (seq r))))])]
     (is (= (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"} {:id 3 :name "Carl"}})
           (assign! rvar (union @rvar (rel {:id 3 :name "Carl"})))))
     (is (empty? (:involved-relvars (meta rvar))))
     (is (thrown? IllegalArgumentException (assign! rvar (union @rvar (rel {:id 1  :name "Dora"})))))
     (is (thrown? IllegalArgumentException (assign! rvar (union @rvar (rel {:id 42 :name "Dora"})))))))
  
  (testing "Assignment of relation with constraints depending on other relations."
    (let [r (rel #{{:pid 1 :phone "123456789"}}) 
          rvar1 (relvar (rel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"}}))
          rvar2 (relvar r
                  [(relfn [r] (every? #(in? (project @rvar1 [:id]) {:id %}) (:pid r)))])]
      (is (= (union r (rel {:pid 2 :phone "987654321"}))
             (assign! rvar2 (union @rvar2 (rel {:pid 2 :phone "987654321"})))))
      (is (= #{'rvar1} (:involved-relvars (meta rvar2))))
      (is (thrown? IllegalArgumentException (assign! rvar2 (union @rvar2 (rel {:pid 4 :phone "32132"}))))))))