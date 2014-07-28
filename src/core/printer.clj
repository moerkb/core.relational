(ns core.relational)
; pretty printer for relations

(defn print-relation
  "Pretty prints the relation to standard output."
  [relation]
  (pp/print-table relation))