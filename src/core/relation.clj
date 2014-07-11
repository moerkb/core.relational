(ns core.relational)
; definition for relations

(declare sort-rel)

(deftype Relation [head body]
  Object
  (equals [this obj]
    (cond
      (identical? this obj)
      true
      
      (not (instance? Relation obj))
      false
      
      (not= (count (.head this)) (count (.head obj)))
      false

      (and (same-type? this obj)
           (or (and (nil? (.body this)))
               (= (.body this) (.body (sort-rel this obj)))))
      true 
      
      :else false))
  (hashCode [this]
    (+ (.hashCode (.head this))
      (* 37N (.hashCode (.body this)))))
  
  clojure.lang.IKeywordLookup
  (getLookupThunk [this key]
    (reify clojure.lang.ILookupThunk
      (get [_ target]
        (case key
          :body (.body this)
          :head (.head this)
          (let [target-pos (index-of (.head target) key)]
            (if (nil? target-pos)
                nil
                (vec (map (fn [t]
                           (get t target-pos))
                         (.body target))))))))))

(defn sort-rel
  "If both relations have the same type, a relation equal two rel2 is returned
  with the same attribute order as rel1. If they have different type, it just
  returns rel2."
  [rel1 rel2]
  (if-not (same-type? rel1 rel2)
    rel2
    (if (same-attr-order? rel1 rel2)
      rel2
      (let [sorter (sort-vec rel1 rel2)]
        (Relation. 
          (vec (map (fn [a] (get (.head rel2) a)) sorter))
          (set (map (fn [t] (vec (map (fn [a]
                                        (get t a)) sorter))) (.body rel2)))))
      )))

(defn new-relation
  "Give value tuples as a set of hash maps and it creates the relation. If it
  is only one tuple, you may give it as a hash map directly instead as a set."
  [tuple-set]
  (let [tuples (if (or (empty? tuple-set) (nil? tuple-set))
                 #{}
                 (if (map? tuple-set) #{tuple-set} tuple-set))]
    (let [head (vec (keys (first tuples)))]
     (Relation. 
       head
       (set (map (fn [tuple]
                   (vec (map (fn [attr]
                               (get tuple attr))
                             head))) 
                 tuples))))))

(defn rel-to-hash-map
  "Returns a relation as a set of hash maps."
  [rel]
  (set (map (fn [tuple]
              (apply merge (map (fn [attr val]
                                  {attr val})
                                (.head rel)
                                tuple)))
            (.body rel))))

(defmethod print-method Relation 
  [rel writer]
  #_(.write writer (str "Relation" \newline
                       "Header: " (.head rel) \newline 
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
