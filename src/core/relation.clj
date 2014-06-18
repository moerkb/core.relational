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
    #{ [1 \"Arthur\"] [2 \"Betty\"] })

  head: vector of symbols
  body: set of vectors (tuples) that contain values (arbitrary datatype)
        or empty set

  Nowhere may nil appear! (this is not SQL)"
  [head body]
  {:pre [(and 
           ; head is vector of symbols, not nil
           (vector? head)
           (every? symbol? head)
           
           ; body is empty set or set of vectors of values not nil
           (set? body)
           (or
             (empty? body)
             (every? #(and (vector? %) (not-any? nil? %)) body)))]}
  (Relation. head body))