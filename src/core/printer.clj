(ns core.relational)
; pretty printer for relations

(defn print-relation
  "Pretty prints the relation to standard output."
  [relation]
  (let [head (:head relation)
        body (:body relation)
        ; find count for longest item in each column
        my-count (fn [elem]
                   (if (coll? elem)
                     (count elem)
                     (count (str elem))))
        lengths (map #(+ 2 %) (apply map max (map #(map my-count %) 
                                             (conj body head))))
        nof-cols (count head)]
    
    ; print header
    (print "+")
    (doseq [item lengths]
      (dotimes [_ item]
        (print "-"))
      (print "+"))
    (print \newline)
    
    ; column headers
    (print "|")
    (dotimes [i nof-cols]
      (print " ")
      (print (nth head i))
      (dotimes [_ (- (nth lengths i) 
                    (my-count (nth head i))
                    1)]
        (print " "))
      (print "|"))
    (print \newline)
    
    (print "+")
    (doseq [e lengths]
      (dotimes [_ e]
        (print "-"))
      (print "+"))
    (print \newline)
    
    ; print rows
    (doseq [row body]
      (print "|")
      (dotimes [i nof-cols]
        (print " ")
        (print (nth row i))
        (dotimes [_ (- (nth lengths i) 
                      (my-count (nth row i))
                      1)]
          (print " "))
        (print "|"))
    (print \newline))
    
    ; print footer
    (print "+")
    (doseq [item lengths]
      (dotimes [_ item]
        (print "-"))
      (print "+"))
    (print \newline)
    ))