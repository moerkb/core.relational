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
    "Returns a relation with only the tuples that satisfy the predicate. You
    can use a single argument function for it (argument is a single tuple), but
    you should use 'restr-pred' instead, as it can be optimized. Be aware that 
    every keyword in pred will be interpreted as an attribute. 

    Examples:
      (restrict r (restr-pred (= :sno 12)))  ; preferred

      (restrict r (fn [t] (= (:sno t) 12)))  ; same as
      (restrict r #(= (:sno %) 12))")
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
      (project r {:sno :sno, :new-status '(* 2 :status)})")
  (project- [relation attributes]
    "Projects the relation with all original attributes, but the one specified.
    Think of it as \"remove\".

    Example:
      (project- r #{:sno})  ; relation r without :sno")
  (extend [relation extend-map]
    "Extends the relation with the attributes specified in extend-map. In this,
    a key is a new attribute and the value a single argument function that
    retrieves the tuple and returns the new value. The same effect can be
    achieved with project.

    Examples:
      (extend r {:new-price (fn [t] (* 1.05 (:price t)))})
      ; same as
      (project r {:a1 :a1, :a2 :a2, ..., :new-price #(* 1.05 (:price t))})")
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
    "Counter part to group: extracts the attributes (that must be relations) to
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
    take a relation (not a tuple!) as their only parameter. The return value
    appears in the resulting relation under the key.

    Examples:
      (summarize r #{:sno} {:pcount #(count %)})
      ; like in SQL: \"select sno, count(*) as pcount from r group by sno;\"

      (summarize r #{:pno} {:psum #(reduce + (:price %))})
      ; like in SQL: \"select pno, sum(price) as psum from r group by pno;\""))


; implementation for Relation (clojure data structures, row-oriented)
(extend-protocol RelationalOperators Relation
  (rename [relation smap]
    (Relation. (replace smap (.head relation)) (.body relation)))
  
  (rename* [relation match-exp replace-str]
    (create-relation (vec (map (fn [a] 
                                 (-> a str (subs 1) (str/replace match-exp replace-str) keyword))
                               (.head relation)))
                     (.body relation)))
  
  (restrict [relation predicate?]
    ; TODO
    relation)
  
  (project [relation attributes]
    (if (map? attributes)
      ; attributes is a hash map
      (let [head (vec (keys attributes))
            ; seq with functions that return the correct value for the position
            pos-funs (map (fn [elem]
                              (list 'fn '[t] (walk/postwalk #(if (keyword? %)
                                                               (let [pos (index-of (.head relation) %)]
                                                                 (if pos
                                                                   (list 'get 't pos)
                                                                   %))
                                                               %)
                                                            elem)))
                          (vals attributes))
            body (set (map (fn [t]
                            (vec (map #((eval %) t) pos-funs)))
                          (.body relation)))]
        (create-relation head body))
      
      ; attributes is a set/vector/list
      (let [; find positions of attributes that shall be shown
            positions (remove nil? (map #(index-of (.head relation) %) attributes))
            ; "take" just these attributes
            value-tuples (set (map #(vec (map (fn [p] (nth % p)) positions)) 
                                (.body relation)))
            body (if (every? empty? value-tuples) #{} value-tuples)
            head (vec (map #(get (.head relation) %) positions))] 
        (create-relation head body))))
  
  (join [relation1 relation2]
    (let [common (common-attr relation1 relation2)
          div-r1 (diverging-attr relation1 relation2)
          div-r2 (diverging-attr relation2 relation1)] 
      (cond 
       ; case 1: natural join, case 4: semi join
       (and 
         (not (empty? common))
         (or (not (empty? div-r1))
             (not (empty? div-r2))))
    
       (let [new-head (vec (concat (.head relation1) div-r2))
             common-positions-r1 (map #(index-of (.head relation1) %) common)
             common-positions-r2 (map #(index-of (.head relation2) %) common)
             div-positions-r2 (map #(index-of (.head relation2) %) div-r2)]
         ; add relation 2 value tuples to that of relation 1
         (create-relation new-head 
           ; tuple join
           (set (map (fn [tuple-r1]
                       (first (remove nil? (map (fn [tuple-r2]
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
                                                    (.body relation2)))))
                        (.body relation1)))))
       
       ; case 2: cross join
       (empty? common)
       
       (create-relation (vec (concat (.head relation1) (.head relation2)))
         (set (map vec (apply concat (map (fn [tuple-r1]
                                           (map (fn [tuple-r2]
                                                 (concat tuple-r1 tuple-r2))
                                            (.body relation2)))
                                   (.body relation1))))))
       
       ; case 3: intersect
       (and 
         (not (empty? common))
         (empty? div-r1)
         (empty? div-r2))
       
       (intersect relation1 relation2)
       
       ; default case
       :else (throw (InternalError. "This should never happen (error code 1).")))))
  
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
      (create-relation (.head relation1) (clj-set/union (.body relation1) rel2-body))))
  
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
      (create-relation (.head relation1) (clj-set/intersection (.body relation1) rel2-body))))
  
  #_(group [relation attributes alias]
     (let [positions (map (fn [attr]
                            (index-of (.head relation) attr))
                       attributes)
           remaining (filter (fn [pos]
                               (if (some #(= pos %) positions)
                                 false
                                 true))
                       (range 0 (count (.head relation))))
           new-header (conj (vec (map #(get (.head relation) %) remaining)) alias)
           tuples-rel (apply merge-with union (map (fn [tuple]
                                                     {(vec (map (fn [pos] (get tuple pos)) remaining))
                                                      (create-relation attributes #{(vec (map #(get tuple %)
                                                                                                   positions))})})
                                                (.body relation)))
           new-body (set (map (fn [[k v]] (conj k v)) tuples-rel))]
       (create-relation new-header new-body))))