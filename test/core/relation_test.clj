(ns core.relational-test)

(deftest new-relation-test
  (is (= (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]})
         (new-relation #{ {:id 1 :name "Arthur"} {:id 2 :name "Betty"} }))))

(deftest rel-seq-test
  (is (= #{ {:id 1 :name "Arthur"} {:id 2 :name "Betty"} })
         (seq (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]}))))

(deftest relation-test
  (testing "Relation creation"
    (let [head [:id :name]
          body #{ [1 "Arthur"] [2 "Betty"] }]
      (is (= (core.relational.Relation. head body)
             (create-relation head body)))
      (is (= (core.relational.Relation. [] #{})
             (create-relation [] #{})))))

  (testing "Relation equality"
    (is (= (create-relation [] #{})
           (create-relation [] #{})))
    (is (= (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]})
           (create-relation [:id :name] #{[2 "Betty"] [1 "Arthur"]})))
    (is (not= (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]})
              (create-relation [:id :name] #{[1 "Arthur"]})))
    (is (not= (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]})
              (create-relation [:id :name] #{[1 "Arthur"] [2 "Bethy"]})))
    (is (= (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]})
           (create-relation [:name :id] #{["Arthur" 1] ["Betty" 2]})))))

(deftest sort-rel-test
  (let [r1 (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"]})
        r2 (create-relation [:name :id] #{["Arthur" 1] ["Betty" 2]})]
    (is (and (= (.head r1) (.head (sort-rel r1 r1)))
             (= (.body r1) (.body (sort-rel r1 r1)))))
    (is (and (= (.head r2) (.head (sort-rel r2 r2)))
             (= (.body r2) (.body (sort-rel r2 r2)))))
    (is (and (= (.head r1) (.head (sort-rel r1 r2)))
             (= (.body r1) (.body (sort-rel r1 r2)))))
    (is (and (= (.head r2) (.head (sort-rel r2 r1)))
             (= (.body r2) (.body (sort-rel r2 r1)))))))

(deftest count-rel-test
  (is (= 0 (count (new-relation {}))))
  (is (= 0 (count (new-relation nil))))
  (is (= 0 (count (create-relation [:id :name] nil))))
  (is (= 0 (count (create-relation [:id :name] #{}))))
  
  (is (= 2 (count (new-relation #{{:name "Arthur"} {:name "Betty"}}))))
  (is (= 3 (count (create-relation [:id :name] #{[1 "Arthur"] [2 "Betty"] [3 "Carl"]})))))