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
    (when (not-any? #(= attribute %) (:head relation))
      (throw (IllegalArgumentException. "Attribute does not exist in relation.")))
    (let [new-head (replace {attribute new-name} (:head relation))]
      (create-relation new-head (:body relation))))
  
  (restrict [relation predicate?]
    ; TODO
    relation)
  
  (project [relation attributes]
    ; TODO
    relation)
  
  (join [relation1 relation2]
    ; TODO
    relation1)
  
  (union [relation1 relation2]
    ; TODO
    relation1)
  
  (intersect [relation1 relation2]
    ; TODO
    relation1))