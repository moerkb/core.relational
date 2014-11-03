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

(defn- check-constraints
  "Checks given constraints for given relational value. Throws an exeption if
  a constraint is violated, otherwise nil."
  [relval constraints]
  (doseq [c constraints]
    (when-not (c relval)
      (throw (IllegalArgumentException. 
               (str "The new value does not satisfy the constraint " (:body (meta c))))))))

(defn relvar 
  "Creates a relation variable from the given relation (value). Constraints is 
  a collection of predicates, the relvar always has to satisfy when its value 
  is changed. Each such predicate takes the relation value as its only argument.

  "
  ([relation]
    (ref relation :meta {:constraints nil, :involved-relvars []}))
  ([relation constraints]
    (check-constraints relation constraints)
    (ref relation :meta {:constraints constraints, :involved-relvars (involved-relvars constraints)})))

(defn assign!
  "Assigns a new relation value to a relation variable, but only if it has the
  same type and if all constraints are satisfied. Otherwise, an 
  IllegalArgumentException is thrown and the relvar remains unchanged."
  [rvar new-relation]
  (dosync 
    (when-not (= (scheme @rvar) (scheme new-relation))
      (throw (IllegalArgumentException. "The new value has a different type.")))
    
    (let [rvars (:involved-relvars (meta rvar))
          constraints (remove nil? (concat (:constraints (meta rvar))
                                     (map #(:constraints (meta %)) rvars)))]
      
      ; ensure not possible with globally defined symbols?
      ; (doall (map (comp ensure eval) rvars))
      (ref-set rvar new-relation)
      (check-constraints @rvar constraints)
      @rvar)))

(defn insert!
  "Inserts the tuple (or set of tuples) into relvar."
  [rvar tuple]
  (let [new-rel (rel tuple)] 
    (assign! rvar (union @rvar new-rel))))

(defn delete!
  "Deletes tuples from relvar, for which the tuple predicate returns true. Use 
  relfn to produce the predicate to enable optimization. It takes a single tuple
  as its argument."
  [rvar pred?]
  (let [dif-rel (restrict @rvar pred?)]
    (assign! rvar (difference @rvar dif-rel))))

(defn update!
  "Updates tuples in relvar, for which the tuple predicate is true. The
  value at attribute is then changed to new-value. This can be a fixed value or
  a tuple function. Use relfn for predicate."
  [rvar pred? attribute new-value]
  (assign! rvar (rel (set (map (fn [t]
                                 (if (pred? t)
                                   (assoc t attribute (if (fn? new-value) (new-value t) new-value))
                                   t)) 
                            (seq @rvar))))))