(ns core.relational)

(defn- check-constraints
  "Checks constraints and references of a relvar."
  [rvar]
  (doseq [r (:referenced-by (meta rvar))]
    (check-constraints r))
  (doseq [c (:constraints (meta rvar))]
   (if (map? c)
     ; c is a hash map
     (let [[ctype attr] (first c)
           attr-set (if (keyword? attr) #{attr} attr)]
       (if (not= (count c) 1)
         (throw (IllegalArgumentException. (str "Only one element may be in a constraint hash map: " c)))
         (case ctype
           :unique (when-not (= (count @rvar) (count (project @rvar attr-set)))
                     (throw (IllegalArgumentException. (str "The attribute " attr " is not unique in " @rvar))))
            
           :primary-key (when-not (= (count @rvar) (count (project @rvar attr-set)))
                                     (throw (IllegalArgumentException. (str "This primary key already exists."))))
            
           :foreign-key (when-not (every? #(in? (project @(:referenced-relvar attr) #{(:referenced-key attr)})
                                                {(:referenced-key attr) %})
                                          (map (comp first vals) (project @rvar #{(:key attr)}))) 
                          (throw (IllegalArgumentException. 
                                   (str "The key given for " 
                                        (:key attr) 
                                        " does not appear in the referenced relvar at "
                                        (:referenced-key attr)))))
            
           (throw (IllegalArgumentException. (str "Unkown type in constraint hash map: " c))))))
      
     ; c is predicate, just invoke
     (when-not (c @rvar)
      (throw (IllegalArgumentException. 
               (str "The new value does not satisfy the constraint " (:body (meta c)))))))))

(defn- add-reference!
  "Tell rvar it is referenced by referencer."
  [rvar referencer]
  (alter-meta! rvar assoc :referenced-by (conj (:referenced-by (meta rvar)) referencer)))

(defn- remove-reference!
  "Tell rvar it is no longer referenced by referencer."
  [rvar referencer]
  (alter-meta! rvar assoc :referenced-by (disj (:referenced-by (meta rvar)) referencer)))

(defn relvar 
  "Creates a relation variable from the given relation (value). Constraints is 
  a collection of predicates, the relvar always has to satisfy when its value 
  is changed. Each such predicate takes the relation value as its only argument."
  ([relation]
    (ref relation :meta {:constraints nil, :referenced-by #{}}))
  ([relation constraints]
    (let [constraints (if (or (map? constraints) (fn? constraints)) 
                        [constraints] 
                        constraints)
          references (remove nil? (map #(when (and (map? %) (= :foreign-key (first (keys %))))
                                         (-> % vals first :referenced-relvar)) 
                                       constraints))
          rvar (ref relation :meta {:constraints constraints, :referenced-by #{}})]
      
      
      (check-constraints rvar)
      ; every relvar this one references to, is "notified"
      (doseq [r references]
        (add-reference! r rvar))
      rvar)))

(defn assign!
  "Assigns a new relation value to a relation variable, but only if it has the
  same type and if all constraints are satisfied. Otherwise, an 
  IllegalArgumentException is thrown and the relvar remains unchanged."
  [rvar new-relation]
  (dosync 
    (when-not (= (scheme @rvar) (scheme new-relation))
      (throw (IllegalArgumentException. "The new value has a different type.")))
    
    (ref-set rvar new-relation)
    (check-constraints rvar)
    @rvar))

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

(defn constraint-reset!
  "If the new constraints are valid for relvar, it sets them permanently for it."
  [rvar constraints]
  (dosync 
    (let [constraints (if (or (map? constraints) (fn? constraints))
                        [constraints]
                        constraints)
          old-constraints (:constraints (meta rvar))]
      (alter-meta! rvar assoc :constraints constraints)
      (try 
        (check-constraints rvar)
        (catch Exception e 
          (do (alter-meta! rvar assoc :constraints old-constraints)
              (throw e))))
      
      ; constraints ok, take care of references
      (let [find-references (fn [cs] (set (remove nil? (map #(when (and (map? %) 
                                                                   (= :foreign-key (first (keys %))))
                                                          (-> % vals first :referenced-relvar)) 
                                                       cs))))
            old-refs (find-references old-constraints)
            new-refs (find-references constraints)]
        (doseq [r (clj-set/difference old-refs new-refs)]
          (remove-reference! rvar r))
        (doseq [r (clj-set/difference new-refs old-refs)]
          (add-reference! rvar r))))))

(defn add-constraint!
  "Adds the constraint (see relvar) to a relvar. If the new constraint cannot
  be satisfied by the relvar's value, an exception is thrown and the contraint
  is not added."
  [rvar new-constraint]
  (let [old-cons (:constraints (meta rvar))]
    (constraint-reset! rvar (conj old-cons new-constraint))))