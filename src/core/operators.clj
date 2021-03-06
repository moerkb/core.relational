(ns core.relational)
; operators for relations and relvars

(defprotocol RelationalOperators
  "Protocol for relational operators. If an attribute is given that does not
  exist in the relation, it shall be ignored and not lead to an error.

  If a set of attributes shall be given, it might also be another collection,
  like a vector or a list."
  (rename [relation replace-map] 
    "Given a substitution map, it renames the attributes.
    
    Example:
      (rename r {:pname :product-name})")
  (rename* [relation match-regexp replace-str]
    "Renames all attributes that match match-regexp with replace-str. Semantics
    are the same as clojure.string/replace.

    Example:
      (rename* r #\"(.+)\" \"prefix-$1\"")
  (restrict [relation predicate?] 
    "Returns a relation with only the tuples that satisfy the predicate pred?.
    That is a usual function, but fn shall be replaced with relfn, so that 
    optimization can be done.

    Examples:
      (restrict r (relfn [t] (= (:sno t) \"S12\")))")
  (project [relation project-map] 
    "Returns the relation with only the attributes specified in pmap. That is a
    hash map where the key is the final name and the value is what shall be
    projected. This can be an attribute (can be renamed) or a list, representing
    a function. Attributes must be written as a keyword and every keyword will
    be interpreted as an attribute, if it appears in the original relation. If 
    it is a function, it takes a single argument representing a tuple.

    For convenience, project-map can also be a set of attributes, that shall be
    contained in the resulting relation, but now functions or new names can be
    given.

    Examples:
      (project r {:sno :sno, :supplier-city :city})
      (project r #{:sno :city})
      (project r {:sno :sno, :new-status (relfn [t] (* 2 (:status t)))})")
  (project- [relation attributes]
    "Projects the relation with all original attributes, but the one specified.
    Think of it as \"remove\".

    Example:
      (project- r #{:sno})  ; relation r without :sno")
  (project+ [relation extend-map]
    "Extends the relation with the attributes specified in extend-map. In this,
    a key is a new attribute and the value a tuple function. The same effect can
    be achieved with project.

    Examples:
      (project+ r {:new-price (relfn [t] (* 1.05 (:price t)))})
      ; same as
      (project r {:a1 :a1, :a2 :a2, ..., :an :an, 
                  :new-price (relfn [t] (* 1.05 (:price t))")
  (join [relation1 relation2] 
    "Produces the natural join of both relations.")
  (compose [relation1 relation2]
    "Like join, but the attribute(s) over which is joined are not in the
    resulting relation.")
  (union [relation1 relation2]
    "Combines both relations, provided they have the same type.")
  (intersect [relation1 relation2]
    "Tuples of the returned relation appear in both relations. They must be of
    the same type.")
  (difference [relation1 relation2]
    "If both relations have the same type, it returns relation1 without the
    tuples of relation2.")
  (divide [relation1 relation2]
    "Divide relation1 by relation2.")
  (tclose [binary-relation]
    "Builds the transitive closure on a binary relation.")
  (group [relation group-map]
    "Groups the attributes (group-map values) in a new relation that appears in 
    the original relation as the key.
    
    NOTICE: Name the attributes that shall appear in the group, not by which
            it shall be grouped (as done in SQL).

    Example: Given the relation 'orders' like
    +--------+-----------+-----+
    | BillId | ProductId | Qty |
    +--------+-----------+-----+
    | 7      | 42        | 5   |
    | 5      | 21        | 7   |
    | 5      | 42        | 3   |
    +--------+-----------+-----+

    the statement (group orders {:Positions #{:ProductId :Qty}}) would produce:
    +--------+---------------------+
    | BillId | Positions           |
    +--------+---------------------+
    | 7      | +-----------+-----+ |
    |        | | ProductId | Qty | |
    |        | +-----------+-----+ |
    |        | | 42        | 5   | |
    |        | +-----------+-----+ |
    |        |                     |
    | 5      | +-----------+-----+ |
    |        | | ProductId | Qty | |
    |        | +-----------+-----+ |
    |        | | 21        | 7   | |
    |        | | 42        | 3   | |
    |        | +-----------+-----+ |
    +--------+---------------------+

    In SQL you would say \"GROUP BY BillId\".")
  (ungroup [relation attributes]
    "Inverts group: extracts the attributes (that must be relations) to
    be standard rows again (like you have never done a group).

    Example:
      (ungroup orders #{:Positions})  ; see group")
  (wrap [relation wrap-map]
    "Makes one attribute from several others as specified in wrap-map. The value
    is a set of attributes; the key under which they shall appear.

    Example:
      (wrap r {:address #{:street :zip :city})")
  (unwrap [relation attributes]
    "Takes every attribute from the set attributes and unwraps it, so former
    wrapped attributes are single attributes again like they have never been
    wrapped.

    Example:
      (unwrap r #{:address})  ; see wrap")
  (summarize [relation group-by sum-map]
    "Apply aggregate functions to the relation. group-by is a set of attributes
    by which the result shall be grouped. sum-map's values are functions that 
    take a relation body (not a tuple!) as their only parameter. The return 
    value appears in the resulting relation under the key.

    Examples:
      (summarize r #{:sno} {:pcount (relfn [r] (count r))})
      ; like in SQL: \"select sno, count(*) as pcount from r group by sno;\"

      (summarize r #{:pno} {:psum (relfn [r] (reduce + (:price r)))})
      ; like in SQL: \"select pno, sum(distinct price) as psum from r 
                       group by pno;\""))


; implementation for Relation (clojure data structures, row-oriented)
(extend-protocol RelationalOperators Relation
  (rename [relation smap]
    (Relation. (replace smap (.head relation)) (.body relation)))
  
  (rename* [relation match-exp replace-str]
    (rel (vec (map (fn [a] 
                                 (-> a name (str/replace match-exp replace-str) keyword))
                               (.head relation)))
                     (.body relation)))
  
  (restrict [relation predicate]
    (rel (set (filter predicate (seq relation)))))
  
  (project [relation attributes]
    (if (map? attributes)
      ; attributes is a hash map
      (let [head (vec (keys attributes))
            ; seq with functions that return the correct value for the position
            new-rel (set (map (fn [t]
                                (apply merge (map (fn [[k v]] {k (if (or (keyword? v) (fn? v)) 
                                                                   (v t)
                                                                   v)}) attributes)))
                              (seq relation)))]
        (rel new-rel))
      
      ; attributes is a set/vector/list
      (let [; find positions of attributes that shall be shown
           positions (remove nil? (map #(index-of (.head relation) %) attributes))
           ; "take" just these attributes
           value-tuples (set (map #(vec (map (fn [p] (nth % p)) positions)) 
                                 (.body relation)))
           head (vec (map #(get (.head relation) %) positions))] 
        (rel head value-tuples))))
  
  (project- [relation attributes]
    (let [attrs (if (set? attributes) attributes (set attributes))
          pos (remove nil? (map #(if (contains? attrs %)
                                    nil
                                    (index-of (.head relation) %)) 
                                (.head relation)))]
      (rel (vec (map #(get (.head relation) %) pos))
                       (set (map (fn [t]
                                   (vec (map #(get t %) pos)))
                                 (.body relation))))))
  
  (project+ [relation extend-map]
    (rel (set (map (fn [t]
                        (apply merge t (map (fn [[k v]] {k (if (or (keyword? v) (fn? v)) 
                                                             (v t)
                                                             v)}) extend-map)))
                      (seq relation)))))
  
  (join [relation1 relation2]
    (let [common (common-attr relation1 relation2)
          div-r2 (diverging-attr relation2 relation1)
          new-head (vec (concat (.head relation1) div-r2))
          common-positions-r1 (map #(index-of (.head relation1) %) common)
          common-positions-r2 (map #(index-of (.head relation2) %) common)
          div-positions-r2 (map #(index-of (.head relation2) %) div-r2)] 
      ; add relation 2 value tuples to that of relation 1
      (rel new-head 
        ; tuple join
        (set (apply concat (map (fn [tuple-r1]
                                (remove nil? (map (fn [tuple-r2]
                                                  ; check equality of common attributes
                                                  (if (every? true? (map (fn [pos-r1 pos-r2]
                                                                           (= (nth tuple-r1 pos-r1)
                                                                              (nth tuple-r2 pos-r2)))
                                                                      common-positions-r1
                                                                      common-positions-r2))
                                                    ; join tuples
                                                    (vec (concat tuple-r1 (map (fn [pos]
                                                                                 (nth tuple-r2 pos))
                                                                            div-positions-r2)))))
                                                    (.body relation2))))
                                 (.body relation1)))))))
  
  (compose [relation1 relation2]
    (project- (join relation1 relation2) (common-attr relation1 relation2)))
  
  (union [relation1 relation2]
    (when-not (same-type? relation1 relation2)
      (throw (IllegalArgumentException. "The two relations have different types.")))
    
    (let [rel2-body (if (same-attr-order? relation1 relation2)
                      ; same order: nothing todo
                      (.body relation2)
         
                      ; different order: sort the second relation like the first one
                      (set (let [sorter (sort-vec relation1 relation2)]
                             (map (fn [tuple] 
                                    (vec (map (fn [pos] 
                                                (nth tuple pos)) 
                                           sorter))) 
                               (.body relation2)))))]
      (rel (.head relation1) (clj-set/union (.body relation1) rel2-body))))
  
  (intersect [relation1 relation2]
    (when-not (same-type? relation1 relation2)
      (throw (IllegalArgumentException. "The two relations have different types.")))
    
    (let [rel2-body (if (same-attr-order? relation1 relation2)
                      ; same order: nothing todo
                      (.body relation2)
         
                      ; different order: sort the second relation like the first one
                      (set (let [sorter (sort-vec relation1 relation2)]
                             (map (fn [tuple] 
                                    (vec (map (fn [pos] 
                                                (nth tuple pos)) 
                                           sorter))) 
                               (.body relation2)))))]
      (rel (.head relation1) (clj-set/intersection (.body relation1) rel2-body))))
  
  (difference [relation1 relation2]
    (let [rel2-body (if (same-attr-order? relation1 relation2)
                      ; same order: nothing todo
                      (.body relation2)
         
                      ; different order: sort the second relation like the first one
                      (set (let [sorter (sort-vec relation1 relation2)]
                             (map (fn [tuple] 
                                    (vec (map (fn [pos] 
                                                (nth tuple pos)) 
                                           sorter))) 
                               (.body relation2)))))]
      (rel (.head relation1) (clj-set/difference (.body relation1) rel2-body))))
  
  (divide [relation1 relation2]
    (let [r1-only-attrs (diverging-attr relation1 relation2)
          r1-only (project relation1 r1-only-attrs)]
      (difference r1-only 
                  (project (difference (join r1-only relation2) 
                                       relation1) 
                           r1-only-attrs))))
  
  (tclose [relation]
    (let [temp (keyword (gensym))
          [a1 a2] (.head relation)]
      (loop [r relation]
        (let [new-rel (union r (rename (compose r (rename r {a2 temp, a1 a2})) {temp a2}))]
          (if (= r new-rel)
              r
              (recur new-rel))))))
  
  (group [relation group-map]
    (loop [r relation, gmap group-map] 
      (if (nil? gmap)
        r
        (let [[alias attributes] (first gmap)
              positions (map (fn [attr]
                             (index-of (.head r) attr))
                        attributes)
              remaining (remove (fn [pos]
                                  (some #(= pos %) positions))
                          (range 0 (count (.head r))))
              new-header (conj (vec (map #(get (.head r) %) remaining)) alias)
              tuples-rel (apply merge-with union (map (fn [tuple]
                                                        {(vec (map (fn [pos] (get tuple pos)) remaining))
                                                         (rel (vec attributes) #{(vec (map #(get tuple %)
                                                                                        positions))})})
                                                   (.body r)))
              new-body (set (map (fn [[k v]] (conj k v)) tuples-rel))]
        (recur (rel new-header new-body)
               (next gmap))))))
  
  (ungroup [relation attributes]
    (loop [r relation, attrs attributes]
      (if (nil? attrs)
          r
          (let [attr-pos (index-of (.head r) (first attrs))
                _ (when-not (empty? (clj-set/intersection 
                                      (set (.head r))
                                      (set (.head (get (first (.body r)) attr-pos)))))
                    (throw (IllegalStateException. 
                             "There are attributes in the inner relation that already are in the outer one.")))
                rem-pos  (remove #(= attr-pos %) (range 0 (count (.head r))))
                new-head (vec (concat (remove #(= (first attrs) %) (.head r)) 
                                      (-> (.body r) first (nth attr-pos) .head)))
                new-body (apply concat (map (fn [t]
                                              (let [beginning (map (fn [pos] (nth t pos))
                                                                   rem-pos)]
                                                (map (fn [inner-t]
                                                       (concat beginning inner-t))
                                                     (-> t (nth attr-pos) .body))))
                                            (.body r)))]
            (recur (rel new-head (set (map vec new-body))) 
                   (next attrs))))))
  
  (wrap [relation wrap-map]
    (loop [r relation, wrapper wrap-map]
      (if (nil? wrapper)
          r
          (let [[new-attr old-attrs] (first wrapper)
                old-pos (map #(index-of (.head r) %) old-attrs)
                rem-pos (remove #(index-of old-pos %) (range 0 (count (.head r))))
                new-head (conj (vec (map #(nth (.head r) %) rem-pos)) new-attr)
                new-body (set (map (fn [t]
                                     (conj (vec (map #(nth t %) rem-pos))
                                           (apply merge (map #(hash-map (nth (.head r) %) 
                                                                        (nth t %)) 
                                                             old-pos))))
                                   (.body r)))]
            (recur (rel new-head new-body)
                   (next wrapper))))))
  
  (unwrap [relation attributes]
    (loop [r relation, attrs attributes]
      (if (nil? attrs)
          r
          (let [attr-pos (index-of (.head r) (first attrs))
                _ (when-not (empty? (clj-set/intersection 
                                      (set (.head r))
                                      (set (keys (get (first (.body r)) attr-pos)))))
                    (throw (IllegalStateException. 
                             "There are attributes in the inner relation that already are in the outer one.")))
                rem-pos (remove #(= attr-pos %) (range 0 (count (.head r))))
                new-attrs (-> r .body first (nth attr-pos) keys)
                new-head (vec (concat (map #(nth (.head r) %) rem-pos)
                                      new-attrs))
                new-body (set (map (fn [t] 
                                     (vec (concat (map #(nth t %) rem-pos)
                                                  (map #(get (nth t attr-pos) %) new-attrs))))
                                   (.body r)))]
            (recur (rel new-head new-body)
                   (next attrs))))))
  
  (summarize [relation group-by sum-map]
    (let [group? (not (empty? group-by))
          gsym (keyword (gensym "G_"))
          r (if group? (group relation {gsym (attr-complement relation group-by)}) relation)
          inner-rel-index (index-of (.head r) gsym)]
      (if group?
          ; with group by
          (loop [new-rel r
                 summap sum-map]
            (if (nil? summap)
                (project- new-rel [gsym])
                (let [[name fun] (first summap)
                      new-head (conj (.head new-rel) name)
                      new-body (set (map (fn [t]
                                           (let [new-val (fun (nth t inner-rel-index))] 
                                             (conj t new-val)))
                                         (.body new-rel)))]
                  (recur (rel new-head new-body) 
                         (next summap)))))
          
          ; no group by attributes
          (loop [new-rel (rel [] #{[]})
                 summap sum-map]
            (if (nil? summap)
                new-rel
                (let [[name fun] (first summap)]
                  (recur (project+ new-rel {name (fun r)}) 
                         (next summap)))))))))