(ns core.relational-test)

(deftest rel-test
  (is (= (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
         (rel #{ {:id 1 :name "Arthur"} {:id 2 :name "Betty"} }))))

(deftest rel-seq-test
  (is (= #{ {:id 1 :name "Arthur"} {:id 2 :name "Betty"} })
         (seq (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]}))))

(deftest relation-test
  (testing "Relation creation"
    (let [head [:id :name]
          body #{ [1 "Arthur"] [2 "Betty"] }]
      (is (= (core.relational.Relation. head body)
             (rel head body)))
      (is (= (core.relational.Relation. [] #{})
             (rel [] #{})))))

  (testing "Relation equality"
    (is (= (rel [] #{})
           (rel [] #{})))
    (is (= (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
           (rel [:id :name] #{[2 "Betty"] [1 "Arthur"]})))
    (is (not= (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
              (rel [:id :name] #{[1 "Arthur"]})))
    (is (not= (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
              (rel [:id :name] #{[1 "Arthur"] [2 "Bethy"]})))
    (is (= (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
           (rel [:name :id] #{["Arthur" 1] ["Betty" 2]})))))

(deftest contains-rel-test
  (let [r (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})]
    (is (in? r {:id 1, :name "Arthur"}))
    (is (in? r {:name "Betty", :id 2}))
    (is (not (in? r {:name "Arthur", :id 2})))
    (is (not (in? r {:name "Carl", :id 3})))))

(deftest scheme-test
  (is (= #{} (scheme dee)))
  (is (= #{} (scheme dum)))
  (is (= #{:id :name} (scheme (rel [:id :name] #{[1 "Arthur"]})))))

(deftest body-test
  (is (= #{[]} (body dee)))
  (is (= #{} (body dum)))
  (is (= #{[1 "Arthur"] [2 "Betty"]} (body (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})))))

(deftest sort-rel-test
  (let [r1 (rel [:id :name] #{[1 "Arthur"] [2 "Betty"]})
        r2 (rel [:name :id] #{["Arthur" 1] ["Betty" 2]})]
    (is (and (= (.head r1) (.head (sort-rel r1 r1)))
             (= (.body r1) (.body (sort-rel r1 r1)))))
    (is (and (= (.head r2) (.head (sort-rel r2 r2)))
             (= (.body r2) (.body (sort-rel r2 r2)))))
    (is (and (= (.head r1) (.head (sort-rel r1 r2)))
             (= (.body r1) (.body (sort-rel r1 r2)))))
    (is (and (= (.head r2) (.head (sort-rel r2 r1)))
             (= (.body r2) (.body (sort-rel r2 r1)))))))

(deftest count-rel-test
  (is (= 0 (count (rel {}))))
  (is (= 0 (count (rel nil))))
  (is (= 0 (count (rel [:id :name] nil))))
  (is (= 0 (count (rel [:id :name] #{}))))
  
  (is (= 2 (count (rel #{{:name "Arthur"} {:name "Betty"}}))))
  (is (= 3 (count (rel [:id :name] #{[1 "Arthur"] [2 "Betty"] [3 "Carl"]})))))

(deftest order-test
  (testing "Order by one attribute" 
    (let [r (rel [:id :name] #{[1 "Arthur"] [4 "Betty"] [3 "Carl"]})]
      (is (= (seq [{:id 1, :name "Arthur"} {:id 4, :name "Betty"} {:id 3, :name "Carl"}])
             (seq (order r {:name :asc}))))
      (is (= (reverse (seq [{:id 1, :name "Arthur"} {:id 4, :name "Betty"} {:id 3, :name "Carl"}]))
             (seq (order r {:name :desc}))))
      (is (= (seq [{:id 1, :name "Arthur"} {:id 3, :name "Carl"} {:id 4, :name "Betty"} ])
             (seq (order r {:id :asc}))))
      (is (= (reverse (seq [{:id 1, :name "Arthur"} {:id 3, :name "Carl"} {:id 4, :name "Betty"} ]))
             (seq (order r {:id :desc}))))))
  (testing "Order by several attributes"
    (let [r (rel [:prename :surname] #{["Arthur" "Jackson"]
                                       ["Bethy" "Miles"]
                                       ["Anton" "Jackson"]
                                       ["Aragorn" "Beavis"]
                                       ["Berta" "Beavis"]})]
      (is (= (seq [{:surname "Beavis",  :prename "Berta"}
                   {:surname "Beavis",  :prename "Aragorn"}
                   {:surname "Jackson", :prename "Arthur"}
                   {:surname "Jackson", :prename "Anton"}
                   {:surname "Miles",   :prename "Bethy"}])
             (seq (order r [{:surname :asc} {:prename :desc}])))))))

(deftest dee-dum-test
  (let [r (rel {:id 1 :name "Arthur"})
        r-true  (project (restrict r #(= "Arthur" (:name %))) nil)
        r-false (project (restrict r #(= "Betty"  (:name %))) nil)]
    (is (=    dee r-true))
    (is (not= dum r-true))
    (is (=    dum r-false))
    (is (not= dee r-false))))