;; Reusable method implementations

;; by Konrad Hinsen
;; last updated May 10, 2010

;; Copyright (c) Konrad Hinsen, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; The method code is taken from clojure.core/defrecord, written
;; by Rich Hickey, with minor modifications.

(ns methods-a-la-carte.implementations
  (:use [methods-a-la-carte.core
	 :only (defcode defimpl expand-for with-type-tag)]))

(defimpl equals-and-hashCode [& fields]
  Object
  (hashCode [this#]
    (-> (quote ~this-type) hash
	~@(expand-for [fld [~@fields]] (hash-combine ~fld))))
  (equals [this# o#]
    (boolean 
      (or (identical? this# o#)
	  (when (identical? ~this-type (class o#))
	    (let [hinted-o# ~(with-type-tag ~this-type o#)]
	      (and  ~@(expand-for [fld [~@fields]]
		        (= ~fld (. hinted-o# ~fld))))))))))

(defimpl metadata [fld]
  clojure.lang.IObj
  (meta [this#]
    ~fld)
  (withMeta [this# m#]
    (new ~this-type ~@(replace {'~fld 'm#} '~this-fields))))

(defimpl lookup [& fields]
  clojure.lang.ILookup
  (valAt [this# k#]
    (.valAt this# k# nil))
  (valAt [this# k# else#] 
    (case k#
      ~@(expand-for [fld [~@fields]] ~(keyword '~fld) ~fld)
      else#)))

(defimpl lookup-with-external-map [extmap-field & fields]
  clojure.lang.ILookup
  (valAt [this# k#]
    (.valAt this# k# nil))
  (valAt [this# k# else#] 
    (case k#
      ~@(expand-for [fld [~@fields]] ~(keyword '~fld) ~fld)
      (get ~extmap-field k# else#))))

(defcode reify-lookup-thunk [fld]
  (reify clojure.lang.ILookupThunk
    (get [thunk# gtarget#]
      (if (identical? (class gtarget#) ~this-type) 
	(. ~(with-type-tag ~this-type gtarget#) ~(keyword '~fld))
	thunk#))))

(defimpl keyword-lookup [& fields]
  clojure.lang.IKeywordLookup
  (getLookupThunk [this# k#]
    (case k#
      ~@(expand-for [fld [~@fields]]
	  ~(keyword '~fld)
	  ~(reify-lookup-thunk ~fld))
      nil)))

(defimpl persistent-map [extmap-field & fields]
  clojure.lang.IPersistentMap
  (count [this#]
    (+ ~(count ~fields) (count ~extmap-field)))
  (empty [this#]
    (throw (UnsupportedOperationException.
	     (str "Can't create empty: " ~(str '~this-type)))))
  (cons [this# e#]
    ((var clojure.core/imap-cons) this# e#))
  (equiv [this# o#]
    (.equals this# o#))
  (containsKey [this# k#]
    (not (identical? this# (.valAt this# k# this#))))
  (entryAt [this# k#]
    (let [v# (.valAt this# k# this#)]
      (when-not (identical? this# v#)
	(clojure.lang.MapEntry. k# v#))))
  (seq [this#]
    (concat [~@(expand-for [fld [~@fields]]
	         (new clojure.lang.MapEntry ~(keyword '~fld) ~fld))]
	    ~extmap-field))
  (assoc [this# gk# gv#]
    (case gk#
      ~@(expand-for [fld [~@fields]]
	  ~(keyword '~fld)
	  (new ~this-type ~@(replace {'~fld 'gv#} '~this-fields)))
      (let [new-extmap (assoc ~extmap-field gk# gv#)]
	(new ~this-type
	     ~@(replace {'~extmap-field 'new-extmap} '~this-fields)))))
  (without [this# k#]
    (if (case k#
	  ~(expand-for [fld [~@fields]] ~(keyword '~fld)) true
	  false)
      (dissoc (with-meta (into {} this#) (meta this#)) k#)
      (let [new-extmap (not-empty (dissoc ~'__extmap ~'k#))]
	(new ~this-type
	     ~@(replace {'~extmap-field 'new-extmap} '~this-fields))))))

(defimpl java-serializable []
  java.io.Serializable)

(defimpl java-map []
  java.util.Map
  (size [this#]
    (.count this#))
  (isEmpty [this#]
    (= 0 (.count this#)))
  (containsValue [this# v#]
    (-> this# vals (.contains v#)))
  (get [this# k#]
    (.valAt this# k#))
  (put [this# k# v#]
    (throw (UnsupportedOperationException.)))
  (remove [this# k#]
    (throw (UnsupportedOperationException.)))
  (putAll [this# m#]
    (throw (UnsupportedOperationException.)))
  (clear [this#]
    (throw (UnsupportedOperationException.)))
  (keySet [this#]
    (set (keys this#)))
  (values [this#]
    (vals this#))
  (entrySet [this#]
    (set this#)))

