(ns core.relational-test)

(deftest same-type?-test
  (testing "Comparing types of two relations"
    (let [rel1 (create-relation '[id name] #{})
          rel2 (create-relation '[name id] #{})
          rel3 (create-relation '[key name] #{})] 
      (is (= true (same-type? rel1 rel2)))
      (is (= false (same-type? rel1 rel3)))
      (is (= false (same-type? rel2 rel3))))))