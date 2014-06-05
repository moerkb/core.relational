(ns core.relational-test)

(deftest same-type?-test
  (testing "Comparing types of two relations"
    (let [rel1 (create-relation '[id name] #{})
          rel2 (create-relation '[name id] #{})
          rel3 (create-relation '[key name] #{})] 
      (is (true? (same-type? rel1 rel2)))
      (is (false? (same-type? rel1 rel3)))
      (is (false? (same-type? rel2 rel3))))))

(deftest attr-exist?-test
  (let [empty-rel (create-relation [] #{})
          rel (create-relation '[id name phone] #{})]
    (testing "Checking for existence of a single attribute"
      (is (true? (attr-exist? rel 'name)))
      (is (false? (attr-exist? rel 'street)))
      (is (false? (attr-exist? empty-rel 'name))))
    
    (testing "Checking for existence of several attributes"
      (is (true? (attr-exist? rel 'name 'phone)))
      (is (true? (attr-exist? rel 'phone 'name)))
      (is (true? (attr-exist? rel 'name 'name)))
      (is (false? (attr-exist? rel 'name 'street)))
      (is (false? (attr-exist? empty-rel 'name 'id))))))

(deftest attr-not-exist?-test
  (let [empty-rel (create-relation [] #{})
          rel (create-relation '[id name phone] #{})]
    (testing "Checking for existence of a single attribute"
      (is (false? (attr-not-exist? rel 'name)))
      (is (true? (attr-not-exist? rel 'street)))
      (is (true? (attr-not-exist? empty-rel 'name))))
    
    (testing "Checking for existence of several attributes"
      (is (false? (attr-not-exist? rel 'name 'phone)))
      (is (false? (attr-not-exist? rel 'phone 'name)))
      (is (false? (attr-not-exist? rel 'name 'name)))
      (is (true? (attr-not-exist? rel 'name 'street)))
      (is (true? (attr-not-exist? empty-rel 'name 'id))))))

(deftest index-of-test
  (let [my-coll '[id name phone]]
    (is (= 0 (index-of my-coll 'id)))
    (is (= 1 (index-of my-coll 'name)))
    (is (= 2 (index-of my-coll 'phone)))
    (is (nil? (index-of my-coll 'street)))))