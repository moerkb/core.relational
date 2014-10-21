(ns core.relational-test)

; not unit testable

; examples for manual testing
#_(def foo (rel [:id :name :phone]
   #{[1 "Arthur" "+49 641 12345"]
     [2 "Betty" "+49 641 54321"]
     [3 "Carlos Christopherus" "+49 176 123456789"]}))

#_(print-relation foo)