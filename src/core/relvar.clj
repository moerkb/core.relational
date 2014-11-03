(ns core.relational)

(defn involved-relvars
  "Takes a collection of constraints and returns a set of all relvars that 
  appear in them."
  [constraints]
  (if (empty? constraints)
    #{}
    (loop [symlist (flatten (map #(:body (meta %)) constraints))
           res #{}]
      (if (empty? symlist)
        res
        (if (and 
              (symbol? (first symlist))
              (= (first symlist) 'clojure.core/deref))
          (recur (drop 2 symlist)
                 (conj res (second symlist)))
          (recur (rest symlist) 
                 res))))))

(defn relvar 
  "Creates a relation variable from the given relation (value). Constraints is 
  a collection of predicates, the relvar always has to satisfy when its value 
  is changed. Each such predicate takes the relation value as its only argument."
  ([relation]
    (ref relation :meta {:constraints nil, :involved-relvars []}))
  ([relation constraints]
    (ref relation :meta {:constraints constraints, :involved-relvars (involved-relvars constraints)})))

(defn assign!
  "Assigns a new relation value to a relation variable, but only if all
  constraints are satisfied. Otherwise, an IllegalArgumentException is 
  thrown and the relvar remains unchanged."
  [relvar new-relation]
  (dosync 
    (if (every? true? (map (fn [p] (p new-relation)) 
                       (:constraints (meta relvar))))
     (ref-set relvar new-relation)
     (throw (IllegalArgumentException. 
              "The new relation value does not satisfy all constraints.")))))