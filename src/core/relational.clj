(ns core.relational
  (:require [clojure.repl :refer :all]
            [clojure.set :as clj-set]))

; entry point for core.relational; loads other classes

(declare same-type?)
(declare same-attr-order?)
(declare sort-vec)

(load "relation")
(load "tools")
(load "operators")
(load "printer")