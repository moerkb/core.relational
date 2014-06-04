(ns core.relational)
; general tools for working with relations

(defn same-type? 
  "Checks if two relations have the same type, i.e. have the same header."
  [relation1 relation2]
  (= 
    (sort (:head relation1))
    (sort (:head relation2))))