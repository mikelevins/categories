;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          c3-domain.scm
;;;; Project:       Categories
;;;; Purpose:       The -c3- domain
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************


(ns xg.categories.domains.c3
  (:refer-clojure :exclude [type])
  (:require xg.categories.utils
            xg.categories.structures
            xg.categories.types
            xg.categories.domains
            xg.categories.functions
            xg.categories
            xg.categories.c3))

(refer 'xg.categories.utils)
(refer 'xg.categories.structures)
(refer 'xg.categories.types)
(refer 'xg.categories.domains)
(refer 'xg.categories.functions)
(refer 'xg.categories)
(refer 'xg.categories.c3)

;;; ----------------------------------------------------------------------
;;; ABOUT
;;; ----------------------------------------------------------------------
;;; The -c3- domain implements subtype/supertype relations with
;;; multiple inheritance, using the C3 linearization algorithm
;;; described here:
;;;
;;; http://192.220.96.201/dylan/linearization-oopsla96.html
;;;
;;; The C3 algorithm is implemented in c3.scm, and may be reused for
;;; other domains. 


;;; First version: a C3 domain with no caching (this will be
;;; inefficient, because many unnecessary precedence-list computations
;;; will take place)

;;; ----------------------------------------------------------------------
;;; Domain data
;;; ----------------------------------------------------------------------

(def <anything> (make-primitive-type '<anything> (fn [x] true)))

(def <c3-domain-data>
  (structure [] (:direct-supertypes :default (ref nil) :setter t)))

(let [standard-c3-types {<anything> '()
                         <type> (list <anything>)
                         <primitive-type> (list <type>)
                         <synonym> (list <type>)
                         <category> (list <type>)
                         <structure> (list <type>)}
      c3-domain-data (make <c3-domain-data> :direct-supertypes (ref standard-c3-types))]
  (defn c3-domain-data [] c3-domain-data))

(defn direct-supertypes-table [dom]
  (deref (get-key (get-key dom :domain-data) :direct-supertypes)))

;;; ----------------------------------------------------------------------
;;; The C3 type graph
;;; ----------------------------------------------------------------------
;;; There are two rules: one for Java Classes, the other for Java
;;; interfaces and Categories types:
;;; - Java classes: if the type is not found in the direct-supers table,
;;;   return the class' superclass. The supertype of java.lang.Object is
;;;   <anything>
;;; - Java interfaces and Categories types: if the type is not found in
;;;   the direct-supers table, signal an unknown-type error

(defn direct-supertypes [dom t]
  (let [types (get (direct-supertypes-table dom) t)]
    (or types
        (if (class? t)
          (if (.isInterface t)
            (error "No such type: %s" t)
            (let [super (.getSuperclass t)]
              (if super (list super)(list <anything>))))
          (error "No such type: %s" t)))))

(defn all-supertypes [dom t]
  (let* [direct-supers-fn (fn [t] (direct-supertypes dom t))
         all-supers-fn (fn [t] 
                         (compute-precedence t 
                                             direct-supers-fn
                                             (fn [x] (all-supertypes dom x))))]
        (compute-precedence t direct-supers-fn all-supers-fn)))

(defn derive-type! [dom tp supertypes]
  (let [supers-table (get-key (get-key dom :domain-data) :direct-supertypes)]
    (dosync
     (alter supers-table
            (fn [t] (merge t {tp supertypes}))))))

(defn remove-type! [dom tp]
  (let [supers-table (get-key (get-key dom :domain-data) :direct-supertypes)]
    (alter supers-table
           (fn [t] (dissoc t tp)))))

;;; ----------------------------------------------------------------------
;;; method selection
;;; ----------------------------------------------------------------------
;;; In C3, a method signature is a sequence of types whose length is
;;; equal to the arity of the function. When the function is applied,
;;; methods are selected by matching each input value against the
;;; corresponding type on the signature. A match is positive if the
;;; input's type is equal to or a subtype of the signature
;;; type. Because matches on subtypes are positive, it's possible for
;;; more than one method to match a given set of inputs, and the
;;; selector must therefore be able to order matching methods
;;; according to specificity. When the function is applied, we wish to
;;; execute the most specific method, and supply less-specific methods
;;; in order of decreasing specificity for the use of next-method
;;; calls. Our standard for specificity is this:
;;; 1. if any argument fails to match a method signature, then there
;;;    are no applicable methods
;;; 2. given two applicable signatures S1 and S2, consisting of types
;;;    T0(s1), T1(s1), ... and  T0(s2), T1(s2), ...
;;;    then if T0(s1) is a subype of T0(s2), then S1 is more specific.
;;;    if T0(s2) is a subype of T0(s1), then S2 is more specific.
;;;    If neither is true, then we can't decide based on the first 
;;;    argument position, and we must next check T1(s1) and T1(s2),
;;;    continuing until a more specific signature is determined.
;;;    If we run out of argument positions without making a determination,
;;;    then dispatch is ambiguous and we must signal an error.

(defn signature-equal? [s1 s2]
  (and (= (count s1)(count s2))
       (let [true? (fn [x] x)]
         (every? true? (map (fn [x1 x2] (= x1 x2)) s1 s2)))))

(defn subtype-of? [dom t1 t2]
  (and (some (fn [t] (= t t2))
             (all-supertypes dom t1))
       true))

;;; return -1 if s1 is more specific, 1 if s2 is more specific,
;;; and signal a dispatch error if specificity cannot be decided
(defn compare-signatures [dom s1 s2]
  (if (or (empty? s1)
          (empty? s2))
    (error "Ambiguous dispatch")
    (let [t1 (first s1)
          t2 (first s2)]
      (if (subtype-of? dom t1 t2)
        -1
        (if (subtype-of? dom t2 t1)
          1
          (compare-signatures dom (rest s1) (rest s2)))))))

(defn order-methods [fun vals]
  (let [dom (get-domain fun)
        mtable (get-method-table fun)
        entries (get-method-entries mtable)
        candidates (map key (filter (fn [entry] (= (count vals)(count (key entry))))
                                    entries))
        vtypes (map type vals)
        true? (fn [x] x)
        applicable-candidates (filter (fn [c] (every? true? (map (fn [vt ct] (subtype-of? dom vt ct))
                                                                 vtypes c)))
                                      candidates)
        ordered-sigs (sort (partial compare-signatures dom) applicable-candidates)
        ordered-meths (map (fn [s] (get entries s false))
                           ordered-sigs)]
    (filter true? ordered-meths)))

(defn c3-select-methods [fun vals]
  (let [meths (order-methods fun vals)]
    (if (empty? meths)
      false
      (let [m (first meths)
            more (rest meths)]
        (if (empty? more)
          {:effective-method m :next-method false :ordered-other-applicable-methods false}
          (let [nm (first more)
                more (rest more)]
            (if (empty? more)
              {:effective-method m :next-method nm :ordered-other-applicable-methods false}
              {:effective-method m :next-method nm :ordered-other-applicable-methods (first more)})))))))

;;; ----------------------------------------------------------------------
;;; The C3 domain
;;; ----------------------------------------------------------------------

(def -c3- (domain :domain-data (c3-domain-data)
                  :method-selector c3-select-methods))


;;; ======================================================================
;;; conveniences
;;; ======================================================================

(defmacro method [params & body]
  (let [vars (map first params)
        quals (map second params)]
    `(make-method [~@quals] (fn [~@vars] ~@body))))
