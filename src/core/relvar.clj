(ns core.relational)

(defn relvar 
  "Creates a relation variable from the given relation (value). Constraints is 
  a collection of predicates, the relvar always has to satisfy when its value 
  is changed. Each such predicate takes the relation value as its only argument."
  ([relation]
    (atom relation :meta {:constraints nil}))
  ([relation constraints]
    (atom relation :meta {:constraints constraints})))

(defn assign!
  "Assigns a new relation value to a relation variable, but only if all
  constraints are satisfied. Otherwise, an IllegalArgumentException is 
  thrown and the relvar remains unchanged."
  [relvar new-relation]
  (if (every? true? (map (fn [p] (p new-relation)) 
                      (:constraints (meta relvar))))
    (reset! relvar new-relation)
    (throw (IllegalArgumentException. 
             "The new relation value does not satisfy all constraints."))))