(ns core.relational)
; general tools for working with relations

(defn same-type? 
  "Checks if two relations have the same type, i.e. have the same header."
  [relation1 relation2]
  (= 
    (sort (:head relation1))
    (sort (:head relation2))))

(defn attr-exist?
  "Checks if the attribute(s) exist in the relation."
  ([relation attribute & more]
    (and 
      (attr-exist? relation attribute)
      (apply attr-exist? relation more)))
  ([relation attribute]
    (if (some #(= attribute %) (:head relation))
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