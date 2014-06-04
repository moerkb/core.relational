(ns core.relational)
; operators for relations and relvars

; main protocol for all relational operators
(defprotocol RelationalOperators
  (rename [relation attribute new-name])
  (restrict [relation condition])
  (project [relation attributes])
  (join [relation1 relation2])
  (union [relation1 relation2])
  (intersect [relation1 relation2]))

; implementation for Relation
(extend-protocol RelationalOperators Relation
  (rename [relation attribute new-name]
    (let [new-head (replace {attribute new-name} (:head relation))]
      (create-relation new-head (:body relation))))
  (restrict [relation condition]
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