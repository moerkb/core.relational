(ns core.relational)
; definition for relations and relation variables (relvars)

(defrecord Relation [head body])

(defn create-relation 
  "Defines a new (typeless) relation. Head is the structure in form
  [attribute1, attribute2, ...] and body of the form #{ [value1-1 value1-2 ...] 
  [value2-1 value2-2 ...] ...}.

  Example:
  (relation
    '[id name]
    #{ [1 \"Arthur\"] [2 \"Betty\"] })"
  [head body-tuples]
  (Relation. head body-tuples))