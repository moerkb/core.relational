;; Dynamically scoped variables

;; by Konrad Hinsen
;; last updated May 1, 2010

;; Copyright (c) Konrad Hinsen, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns methods-a-la-carte.dynamic-scoping)

(def ^:dynamic *dynamic-vars* {})

(defmacro dynamic-let
  [bindings & body]
  (let [var-sym  (gensym "vars")
	bindings (map (fn [[s v]] [var-sym `(assoc ~var-sym (quote ~s) ~v)])
		      (partition 2 bindings))]
    `(let [~var-sym *dynamic-vars*
	   ~@(apply concat bindings)]
       (binding [*dynamic-vars* ~var-sym]
         ~@body))))

(defn dynamic-lookup
  ([sym]
   (dynamic-lookup sym nil))
  ([sym not-found]
   (get *dynamic-vars* sym not-found)))

(defmacro dynamic
  ([sym]
   `(let [v# (dynamic-lookup (quote ~sym) ::not-found)]
      (when (identical? v# ::not-found)
	(throw (Exception. (str "dynamic var " (quote ~sym) " undefined"))))
      v#))
  ([sym not-found]
   `(dynamic-lookup (quote ~sym) ~not-found)))