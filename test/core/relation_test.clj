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

(deftest scheme-test
  (is (= [] (scheme table-dee)))
  (is (= [] (scheme table-dum)))
  (is (= [:id :name] (scheme (newrel [:id :name] #{[1 "Arthur"]})))))

(deftest body-test
  (is (= #{[]} (body table-dee)))
  (is (= #{} (body table-dum)))
  (is (= #{[1 "Arthur"] [2 "Betty"]} (body (newrel [:id :name] #{[1 "Arthur"] [2 "Betty"]})))))

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

(deftest order-test
  (let [r (newrel [:id :name] #{[1 "Arthur"] [4 "Betty"] [3 "Carl"]})]
    (is (= (seq [{:id 1, :name "Arthur"} {:id 4, :name "Betty"} {:id 3, :name "Carl"}])
           (seq (order r {:name :asc}))))
    (is (= (reverse (seq [{:id 1, :name "Arthur"} {:id 4, :name "Betty"} {:id 3, :name "Carl"}]))
           (seq (order r {:name :desc}))))
    (is (= (seq [{:id 1, :name "Arthur"} {:id 3, :name "Carl"} {:id 4, :name "Betty"} ])
           (seq (order r {:id :asc}))))
    (is (= (reverse (seq [{:id 1, :name "Arthur"} {:id 3, :name "Carl"} {:id 4, :name "Betty"} ]))
           (seq (order r {:id :desc}))))))

(deftest dee-dum-test
  (let [r (newrel {:id 1 :name "Arthur"})
        r-true  (project (restrict r #(= "Arthur" (:name %))) nil)
        r-false (project (restrict r #(= "Betty"  (:name %))) nil)]
    (is (=    table-dee r-true))
    (is (not= table-dum r-true))
    (is (=    table-dum r-false))
    (is (not= table-dee r-false))))