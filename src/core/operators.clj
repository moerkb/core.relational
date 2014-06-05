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
    ; TODO
    relation1)
  
  (union [relation1 relation2]
    ; TODO
    relation1)
  
  (intersect [relation1 relation2]
    ; TODO
    relation1))