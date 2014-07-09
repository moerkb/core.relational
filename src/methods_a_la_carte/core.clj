;; Reusable method implementations

;; by Konrad Hinsen
;; last updated May 19, 2010

;; Copyright (c) Konrad Hinsen, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns methods-a-la-carte.core
  (:use [methods-a-la-carte.dynamic-scoping
	 :only (dynamic dynamic-lookup dynamic-let)]))

(declare eval-unquote eval-and-splice)

(defn- expand?
  "Return true if form is expanded normally, or false if form is
   evaluated directly and handles subtemplate expansion itself."
  [form]
  (let [meta-form (get (meta (first form)) :eval-unexpanded)]
    (if meta-form
      meta-form
      (if (and (seq? form)
	       (not (empty? form))
	       (symbol? (first form)))
	(not (get (meta (ns-resolve (dynamic current-ns) (first form)))
		  :eval-unexpanded false))
	true))))

(defn- eval-in-ns
  "Evaluate form in the namespace ns."
  [ns form]
  (let [current-ns (ns-name *ns*)
	_          (in-ns ns)
	v          (eval form)
	_          (in-ns current-ns)]
    v))

(let [not-found (Object.)]
  (defn- eval-expr
    "Evaluate form inside a code template."
    [form]
    (if (symbol? form)
      (let [dv (dynamic-lookup form not-found)]
	(if (identical? dv not-found)
	  (let [var (ns-resolve (dynamic current-ns) form)]
	    (if (nil? var)
	      (throw (Exception. (str "no var " form)))
	      @var))
	  dv))
      (eval-in-ns (dynamic current-ns)
		  (if (expand? form)
		    (eval-unquote form)
		    form)))))

(defn- local-sym
  "Return the local expansion of a symbol ending with #."
  [sym]
  (let [sym-map (dynamic local-syms)
	gsym    (get @sym-map sym)]
    (if (nil? gsym)
      (let [gsym (gensym (str sym))]
	(swap! sym-map assoc sym gsym)
	gsym)
      gsym)))

(defn eval-unquote
  "Evaluate all subforms prefixed by unquote or unquote-splicing inside form."
  [form]
  (let [meta-map  (meta form)
	expanded  (cond
		     (sequential? form)
		       (if (and (seq? form)
				(= (first form) 'clojure.core/unquote))
			 (eval-unquote (eval-expr (second form)))
			 (let [exp (eval-and-splice form)]
			   (cond
			      (list? form) (list* exp)
			      (seq? form)  (doall exp)
			      (vector? form) (vec exp))))
		     (map? form)  (into (if (sorted? form) (sorted-map) {})
					(map eval-unquote form))
		     (set? form)  (into (if (sorted? form) (sorted-set) #{})
					(map eval-unquote form))
		     (and (symbol? form)
			  (= \# (last (str form))))
		       (local-sym form)
		     :else form)]
    (if (nil? meta-map)
      expanded
      (with-meta expanded meta-map))))

(defn- eval-and-splice
  [form]
  (when-let [s (seq form)]
    (let [[f & r] s]
      (if (and (seq? f)
	       (= (first f) 'clojure.core/unquote-splicing))
	(concat (eval-unquote (eval-expr (second f)))
		(eval-and-splice r))
	(cons (eval-unquote f) (eval-and-splice r))))))

(defmacro defcode
  "Define a code template. A code template is a function that returns
   form after substituting subforms prefixed by ~ or ~@ by their values.
   The function arguments args can be used inside these expression at
   an arbitrary nesting level."
  [name args form]
  (let [fn-name         (gensym name)
	individual-args (take-while #(not (= % '&)) args)
        remaining-args  (drop (inc (count individual-args)) args)
	arg-names       (vec (remove #{'&} args))
	remaining       (if (empty? remaining-args)
			  (list)
			  (list `(list 'quote (map #(list 'quote %)
						   ~(first remaining-args)))))
	current-ns      (ns-name *ns*)]
    `(do
       (defn ~fn-name ~arg-names
         (dynamic-let ~(vec (apply concat
				   ['current-ns (list 'quote current-ns)]
				   ['local-syms (list 'atom {})]
				   (map (fn [a] [a a]) arg-names)))
	   (eval-unquote (quote ~form))))
       (defmacro ~name ~args
	 (list (quote ~(symbol (str (ns-name *ns*)) (str fn-name)))
	       ~@(map (fn [a] (list 'list ''quote a)) individual-args)
	       ~@remaining)))))

(defmacro defimpl
  "Define a multi-form code template. Equivalent to defcode with all
   forms in body put into a vector."
  [name args & body]
  `(defcode ~name ~args ~body))

(defmacro deftype+
  "Like Clojure's deftype, but with template expansion in syntax-quote style.
   Defines ~this-type to be the symbol naming the class being defined and
   ~this-fields to be the vector of the type-hinted field name symbols."
  [type fields & methods]
  (cons 'deftype
    (cons type
      (cons (eval-unquote fields)
        (dynamic-let [current-ns  (ns-name *ns*)
		      local-syms  (atom {})
		      this-type   type
		      this-fields fields]
	  (eval-unquote methods))))))

(defmacro with-type-tag
  "Returns form with {:tag type-tag} as metadata."
  [type-tag form]
  `(quote ~(with-meta form {:tag type-tag})))

(defmacro ^{:eval-unexpanded true} expand-for
  "Macro for use in code templates. Works much like Clojure's for."
  [seq-exprs & body]
  (let [seq-exprs (eval-unquote seq-exprs)
	syms      (mapcat #(repeat 2 %)
			  (map first (partition 2 seq-exprs)))]
    `(apply concat
	    (for ~seq-exprs
	      (dynamic-let [~@syms]
			   (eval-unquote (quote ~body)))))))
