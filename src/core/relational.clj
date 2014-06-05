(ns core.relational
  (:require [clojure.repl :refer :all]
            [clojure.set :as clj-set]))

; entry point for core.relational; loads other classes

(load "relation")
(load "tools")
(load "operators")