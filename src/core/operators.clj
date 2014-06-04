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
    "Natural join for two relations. Mind possible degenerations. TODO")
  (union [relation1 relation2]
    "Combines both relations. Relations must be of same type, i.e. have
    same header.")
  (intersect [relation1 relation2]
    "Returns tuples that apper in both relations. The must be of the same
    type, i.e. have same header."))

; implementation for Relation
(extend-protocol RelationalOperators Relation
  (rename [relation attribute new-name]
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