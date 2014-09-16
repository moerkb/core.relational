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
      
      (not= (count (.body this)) (count (.body obj)))
      false

      (and (= (.hashCode this) (.hashCode obj))
           (same-type? this obj)
           (or (and (nil? (.body this)))
               (= (.body this) (.body (sort-rel this obj)))))
      true 
      
      :else false))
  (hashCode [this]
    (let [shead (sort (.head this))
          sbody (sort (map hash (.body (sort-rel (Relation. shead #{}) this))))]
      (+ (* 31 (+ (* 17 31) 
                  (hash shead))) 
         (hash sbody))))
  
  clojure.lang.Seqable
  (seq [this] 
    (map (fn [tuple]
         (apply merge (map (fn [attr val]
                             {attr val})
                           (.head this)
                           tuple)))
       (.body this)))

  clojure.lang.Counted
  (count [this]
    (count (.body this)))
  
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
                (set (map (fn [t]
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

(defmethod print-method Relation 
  [rel writer]
  (.write writer (str "Rel:" (seq rel))))

(defn newrel 
  "Creates a relation in three possible ways:

  (1) Given as a set of hash maps: #{ {:id 1, :name \"Arthur\"} {:id 2, :name \"Betty\"} }
  (2) Given as a single hash map: {:id 1, :name \"Arthur\"}
  (3) Given as head and body: [:id :name] #{ [1 \"Arthur\"] [2 \"Betty\"} } 

  Never use nil!"
  ([head body]
    (Relation. head body))
  
  ([tuple-set]
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
                   tuples)))))))

(defn in?
  "Checks if the tuple is containing in the relation"
  [rel tuple]
  (some #(= % tuple) (seq rel)))

(def table-dee "Represents true." (newrel [] #{[]}))
(def table-dum "Represents false." (newrel [] #{}))

(defn save-rel
  "Saves the relation in the specified file."
  [rel file]
  (spit file (prn-str (set rel))))

(defn load-rel
  "Loads a relation from the specified file."
  [file]
  (newrel (edn/read-string (slurp file))))

(defn order
  "Returns the relation as a sorted set. The sorting is defined by the hash 
  map, with the key as the attribute to be sorted by and its value either :asc
  or :desc. Example: (sort r {:id :asc})"
  [rel sort-map]
  (apply sorted-set-by 
         (fn [r1 r2]
           (let [[attr order] (first sort-map)] 
             (if (= :asc order)
                 (compare (r1 attr) (r2 attr))
                 (compare (r2 attr) (r1 attr))))) 
         (seq rel)))
