(ns core.relational)
; general tools for working with relations

(defn common-attr
  "Returns a vector of all attributes that both relations have in common.
  Order is that in relation 1."
  [relation1 relation2]
  (remove nil? (map #(some #{%} (.head relation2)) 
                 (.head relation1))))

(defn diverging-attr
  "Returns a vector of all attributes that occur in relation 1, but are not
  common with relation 2. Order is that in relation 1."
  [relation1 relation2]
  (let [common (common-attr relation1 relation2)]
    (remove nil? (map #(if (some #{%} (.head relation2))
                         nil
                         %) 
                 (.head relation1)))))

(defn same-type? 
  "Checks if two relations have the same type, i.e. have the same header."
  [relation1 relation2]
  (= 
    (sort (.head relation1))
    (sort (.head relation2))))

(defn same-attr-order?
  "Checks if the attributes of the relations are in the same order. If not or
  if the headers are not equal, it returns false."
  [relation1 relation2]
  (if (and (same-type? relation1 relation2)
        (= (.head relation1) (.head relation2)))
    true
    false))

(defn attr-exist?
  "Checks if the attribute(s) exist in the relation."
  ([relation attribute & more]
    (and 
      (attr-exist? relation attribute)
      (apply attr-exist? relation more)))
  ([relation attribute]
    (if (some #(= attribute %) (.head relation))
      true
      false)))

(defn attr-not-exist?
  "Checks if at least one of the attribute(s) does not exist in the relation."
  [relation & attributes]
  (not (apply attr-exist? relation attributes)))

(defn index-of 
  "Finds the position of the item in the collection. Nil if not in there."
  [coll item]
  (let [res (count (take-while (partial not= item) coll))]
    (if (>= res (count coll))
      nil
      res)))

(defn make-fun
  "Takes a relation and a list. The list is returned as a single argument 
  function in which every keyword that appears as an attribute in the relation
  will be replaced to be the value of that attribute in tuple t, the argument
  of that function.

  Example:
    (make-fun (create-relation [:id :name] #{...})
              '(= 2 :id))
    ; returns a function like
    (fn [t] (= 2 (get t 0)))"
  [relation flist]
  (let [walk-fun #(if (keyword? %)
                      (let [pos (index-of (.head relation) %)]
                        (if pos
                            (list 'get 't pos)
                            %))
                      %)] 
    (eval (list 'fn '[t] (walk/postwalk walk-fun flist)))))

(defn sort-vec
  "Creates a vector showing the positions of the attributes in the second
  relation in the order of the first relation header. So the second relation
  can be ordered like the first one. Example:
  
  header of rel1 = [id name phone]
  header of rel2 = [name phone id]
  (sort-vec rel1 rel2) => [2 0 1]

  Reads like: The new first attribute of rel2 is currently on position 2, etc.

  If the predicate same-attr-order? is true, it will always return [0 1 2 ...]"
  [relation1 relation2]
  (when-not (same-type? relation1 relation2)
    (throw (IllegalArgumentException. "The two relations have different types.")))
  
  (let [h1 (.head relation1)
        h2 (.head relation2)]
    (vec (map #(index-of h2 %) h1))))

(defn attr-complement
  "Returns a vector of all attributes of the relation, except the one(s)
  specified. Parameter attributes may be a single keyword or a collection.
  Unknown attributes are ignored. Result may be empty."
  [relation attributes]
  (if (and (not (keyword? attributes)) (empty? attributes))
      (.head relation)
      (let [attrs (if (coll? attributes) attributes [attributes])]
        (vec (remove #(if (index-of attrs %) true false) (.head relation))))))