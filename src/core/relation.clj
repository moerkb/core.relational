(ns core.relational)
; definition for relations and relation variables (relvars)

(defrecord Relation [head body])

(defn new-relation
  "Give value tuples as a set of hash maps and it creates the relation"
  [tuple-set]
  (let [head (vec (keys (first tuple-set)))]
    (Relation. 
      head
      (set (map (fn [tuple]
                  (vec (map (fn [attr]
                              (get tuple attr))
                            head))) 
                tuple-set)))))

(defn rel-to-hash-map
  "Returns a relation as a set of hash maps."
  [rel]
  (set (map (fn [tuple]
              (apply merge (map (fn [attr val]
                                  {attr val})
                                (:head rel)
                                tuple)))
            (:body rel))))

(defmethod print-method Relation 
  [rel writer]
  #_(.write writer (str "Relation" \newline
                       "Header: " (:head rel) \newline 
                       "Body:   " (rel-to-hash-map rel)))
  (.write writer (str "Rel:" (rel-to-hash-map rel))))

(defn create-relation 
  "Defines a new relation. Head is the structure in form
  [:attribute1, :attribute2, ...] and body of the form #{ [value1-1 value1-2 ...] 
  [value2-1 value2-2 ...] ...}.

  Example:
  (relation
    [:id :name]
    #{ [1 \"Arthur\"] [2 \"Betty\"] })

  head: vector of symbols
  body: set of vectors (tuples) that contain values (arbitrary datatype)
        or empty set

  Nowhere may nil appear! (this is not SQL)"
  [head body]
  (Relation. head body))
