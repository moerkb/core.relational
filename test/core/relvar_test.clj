(ns core.relational-test)

(deftest relvar-test
  (let [r (newrel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"}})]
    (is (= r @(relvar r)))
    (is (= r @(relvar r [(fn [r] (< (:id r) 21))])))))

(deftest assign-test
  (let [rvar (relvar (newrel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"}})
               [(fn [r] (every? #(< % 21) (:id r)))
                (fn [r] (apply distinct? (map :id (seq r))))])]
    (is (= (newrel #{{:id 1 :name "Arthur"} {:id 2 :name "Betty"} {:id 3 :name "Carl"}})
          (assign! rvar (union @rvar (newrel {:id 3 :name "Carl"})))))
    (is (thrown? IllegalArgumentException (assign! rvar (union @rvar (newrel {:id 1  :name "Dora"})))))
    (is (thrown? IllegalArgumentException (assign! rvar (union @rvar (newrel {:id 42 :name "Dora"})))))))