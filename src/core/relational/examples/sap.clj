(ns core.relational.examples.sap
  (require [core.relational :refer :all]))

; Supplier and Parts (SAP) database by Chris Date
; Exercises as from lecture by Prof. Dr. Renz 
; (http://homepages.thm.de/~hg11260/dbs.html 4 and 5)

(def s (create-relation '[sno sname status city]
                        #{["S1" "Smith" 20 "London"]
                          ["S2" "Jones" 10 "Paris"]
                          ["S3" "Blake" 30 "Paris"]
                          ["S4" "Clark" 20 "London"]
                          ["S5" "Adams" 30 "Athens"]}))

(def sp (create-relation '[sno pno qty]
                         #{["S1" "P1" 300]
                           ["S1" "P2" 200]
                           ["S1" "P3" 400]
                           ["S1" "P4" 200]
                           ["S1" "P5" 100]
                           ["S1" "P6" 100]
                           ["S2" "P1" 300]
                           ["S2" "P2" 400]
                           ["S3" "P2" 200]
                           ["S4" "P2" 200]
                           ["S4" "P4" 300]
                           ["S4" "P5" 400]}))

(def p (create-relation '[pno pname color weight city]
                        #{["P1" "Nut"   "Red"   12 "London"]
                          ["P2" "Bolt"  "Green" 17 "Paris"]
                          ["P3" "Screw" "Blue"  17 "Oslo"]
                          ["P4" "Screw" "Red"   14 "London"]
                          ["P5" "Cam"   "Blue"  12 "Paris"]
                          ["P6" "Cog"   "Red"   19 "London"]}))

stop