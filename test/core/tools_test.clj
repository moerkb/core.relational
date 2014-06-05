(ns core.relational-test)

(deftest same-type?-test
  (testing "Comparing types of two relations"
    (let [rel1 (create-relation '[id name] #{})
          rel2 (create-relation '[name id] #{})
          rel3 (create-relation '[key name] #{})] 
      (is (true? (same-type? rel1 rel2)))
      (is (false? (same-type? rel1 rel3)))
      (is (false? (same-type? rel2 rel3))))))

(deftest attr-exists?-test
  (let [empty-rel (create-relation [] #{})
          rel (create-relation '[id name phone] #{})]
    (testing "Checking for existence of a single attribute"
      (is (true? (attr-exists? rel 'name)))
      (is (false? (attr-exists? rel 'street)))
      (is (false? (attr-exists? empty-rel 'name))))
    
    (testing "Checking for existence of several attributes"
      (is (true? (attr-exists? rel 'name 'phone)))
      (is (true? (attr-exists? rel 'phone 'name)))
      (is (true? (attr-exists? rel 'name 'name)))
      (is (false? (attr-exists? rel 'name 'street)))
      (is (false? (attr-exists? empty-rel 'name 'id))))))