(ns core.relational)
; operators for relations and relvars

(defprotocol RelationalOperators
  "Protocol for all relational operators."
  (rename [relation attribute new-name] 
    "Renames attribute of relation to new-name.")
  (restrict [relation predicate?] 
    "Filters value tuples with given predicate.")
  (project [relation attributes] 
    "Only returns the attributes specified in a collection.")
  (join [relation1 relation2] 
    "Natural join, semi join, cross join and intersect for two relations,
    depending on degenerations:
    
    (common attribute: both relation have that attribute,
    (diverging attribute: an attribute one relation has, but not the other)
    
    (1) If both relations have common and diverging attributes, they are
        joined on their common attributes (natural join).
    (2) If both relations do not have at least one common attribute, the 
        cartesian product is build as a relation (cross join).
    (3) If both relations have common but not diverging attributes, the
        intersect is build.
    (4) If both relations have common attributes, but only one has diverging,
        the semi join is built.")
  (union [relation1 relation2]
    "Combines both relations. Relations must be of same type, i.e. have
    same header.")
  (intersect [relation1 relation2]
    "Returns tuples that apper in both relations. The must be of the same
    type, i.e. have same header."))

; implementation for Relation
(extend-protocol RelationalOperators Relation
  (rename [relation attribute new-name]
    (when (attr-not-exist? relation attribute)
      (throw (IllegalArgumentException. "Attribute does not exist in relation.")))
    (let [new-head (replace {attribute new-name} (:head relation))]
      (create-relation new-head (:body relation))))
  
  (restrict [relation predicate?]
    ; TODO
    relation)
  
  (project [relation attributes]
    (when (and
            (not (empty? attributes))
            (apply attr-not-exist? relation attributes))
      (throw (IllegalArgumentException. "At least one of the attributes does not exist in relation.")))
    
    
    (let [; find positions of attributes that shall be shown
          positions (map #(index-of (:head relation) %) attributes)
          ; "take" just these attributes
          value-tuples (set (map #(vec (map (fn [p] (nth % p)) positions)) 
                              (:body relation)))
          body (if (every? empty? value-tuples) #{} value-tuples)] 
      (create-relation attributes body)))
  
  (join [relation1 relation2]
    (let [common (common-attr relation1 relation2)
          div-r1 (diverging-attr relation1 relation2)
          div-r2 (diverging-attr relation2 relation1)] 
      (cond 
       ; case 1: natural join
       (and 
         (not (empty? common))
         (not (empty? div-r1))
         (not (empty? div-r2)))
    
       (let [new-head (vec (concat (:head relation1) div-r2))
             common-positions-r1 (map #(index-of (:head relation1) %) common)
             common-positions-r2 (map #(index-of (:head relation2) %) common)
             div-positions-r2 (map #(index-of (:head relation2) %) div-r2)]
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
                                                    (:body relation2)))))
                        (:body relation1)))))
       
       ; case 2: cross join
       ; case 3: intersect
       (and 
         (not (empty? common))
         (empty? div-r1)
         (empty? div-r2))
       
       (intersect relation1 relation2)
       
       ; case 4: semijoin)
       :else (throw (InternalError. "This should never happen (error code 1).")))))
  
  (union [relation1 relation2]
    (when-not (same-type? relation1 relation2)
      (throw (IllegalArgumentException. "The two relations have different types.")))
    
    (let [rel2-body (if (same-attr-order? relation1 relation2)
                      ; same order: nothing todo
                      (:body relation2)
         
                      ; different order: sort the second relation like the first one
                      (set (let [sorter (sort-vec relation1 relation2)]
                             (map (fn [tuple] 
                                    (vec (map (fn [pos] 
                                                (nth tuple pos)) 
                                           sorter))) 
                               (:body relation2)))))]
      (create-relation (:head relation1) (clj-set/union (:body relation1) rel2-body))))
  
  (intersect [relation1 relation2]
    (when-not (same-type? relation1 relation2)
      (throw (IllegalArgumentException. "The two relations have different types.")))
    
    (let [rel2-body (if (same-attr-order? relation1 relation2)
                      ; same order: nothing todo
                      (:body relation2)
         
                      ; different order: sort the second relation like the first one
                      (set (let [sorter (sort-vec relation1 relation2)]
                             (map (fn [tuple] 
                                    (vec (map (fn [pos] 
                                                (nth tuple pos)) 
                                           sorter))) 
                               (:body relation2)))))]
      (create-relation (:head relation1) (clj-set/intersection (:body relation1) rel2-body)))))