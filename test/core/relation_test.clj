(ns core.relational-test)

(deftest newrel-test
  (is (= (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
         (newrel #{ {:id 1 :name "Arthur"} {:id 2 :name "Betty"} }))))

(deftest rel-seq-test
  (is (= #{ {:id 1 :name "Arthur"} {:id 2 :name "Betty"} })
         (seq (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]}))))

(deftest relation-test
  (testing "Relation creation"
    (let [head [:id :name]
          body #{ [1 "Arthur"] [2 "Betty"] }]
      (is (= (core.relational.Relation. head body)
             (newrel head body)))
      (is (= (core.relational.Relation. [] #{})
             (newrel [] #{})))))

  (testing "Relation equality"
    (is (= (newrel [] #{})
           (newrel [] #{})))
    (is (= (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
           (newrel [:id :name] #{[2 "Betty"] [1 "Arthur"]})))
    (is (not= (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
              (newrel [:id :name] #{[1 "Arthur"]})))
    (is (not= (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
              (newrel [:id :name] #{[1 "Arthur"] [2 "Bethy"]})))
    (is (= (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
           (newrel [:name :id] #{["Arthur" 1] ["Betty" 2]})))))

(deftest contains-rel-test
  (let [r (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})]
    (is (in? r {:id 1, :name "Arthur"}))
    (is (in? r {:name "Betty", :id 2}))
    (is (not (in? r {:name "Arthur", :id 2})))
    (is (not (in? r {:name "Carl", :id 3})))))

(deftest sort-rel-test
  (let [r1 (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
        r2 (newrel [:name :id] #{["Arthur" 1] ["Betty" 2]})]
    (is (and (= (.head r1) (.head (sort-rel r1 r1)))
             (= (.body r1) (.body (sort-rel r1 r1)))))
    (is (and (= (.head r2) (.head (sort-rel r2 r2)))
             (= (.body r2) (.body (sort-rel r2 r2)))))
    (is (and (= (.head r1) (.head (sort-rel r1 r2)))
             (= (.body r1) (.body (sort-rel r1 r2)))))
    (is (and (= (.head r2) (.head (sort-rel r2 r1)))
             (= (.body r2) (.body (sort-rel r2 r1)))))))

(deftest count-rel-test
  (is (= 0 (count (newrel {}))))
  (is (= 0 (count (newrel nil))))
  (is (= 0 (count (newrel [:id :name] nil))))
  (is (= 0 (count (newrel [:id :name] #{}))))
  
  (is (= 2 (count (newrel #{{:name "Arthur"} {:name "Betty"}}))))
  (is (= 3 (count (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"] [3 "Carl"]})))))