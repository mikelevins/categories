;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          categories.clj
;;;; Project:       Categories
;;;; Purpose:       surface API of categories
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(ns com.mikelevins.categories
  (:refer-clojure :exclude [type]))

(refer 'com.mikelevins.categories.utils)
(refer 'com.mikelevins.categories.structures)
(refer 'com.mikelevins.categories.types)
(refer 'com.mikelevins.categories.domains)
(refer 'com.mikelevins.categories.functions)

;;; ======================================================================
;;; Types
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; type definitions
;;; ----------------------------------------------------------------------

(def <structure> (make-primitive-type ::<structure> structure-basis?))
(def <class> (make-primitive-type ::<class> class?))
(def <primitive-type> (make-primitive-type ::<primitive-type> primitive-type?))
(def <synonym> (make-primitive-type ::<synonym> synonym-type?))
(def <category> (make-primitive-type ::<category> category-type?))
(def <type> (make-primitive-type ::<type> type-representation?))

;;; ----------------------------------------------------------------------
;;; type function
;;; ----------------------------------------------------------------------

(defn type [x]
  (cond
    (class? x) <class>
    (structure-instance? x) (basis x)
    (structure-basis? x) <structure>
    (primitive-type? x) <primitive-type>
    (synonym-type? x) <synonym>
    (category-type? x) <category>
    :else (clojure.core/type x)))

;;; ----------------------------------------------------------------------
;;; Structures
;;; ----------------------------------------------------------------------

(defmacro structure 
  ([includes & keyspecs] `(construct-structure-basis (merge-keys (merge-basis-keys ~includes) 
                                                                 (into {} (map parse-keyspec '~keyspecs))))))

(defmulti get-key (fn [i k] (clojure.core/type i)))

(defmethod get-key :default [inst k](error "Not a structure instance: %s" inst))

(defmethod get-key $structure-instance-tag [inst k](k inst))

(defmethod get-key $mutable-structure-instance-tag [inst k]
  (k (deref (:value inst))))

(defmulti set-key! (fn [i k v] (clojure.core/type i)))

(defmethod set-key! :default [inst k v]
  (error "Not a structure instance: %s" inst))

(defmethod set-key! $structure-instance-tag [inst k v]
  (error "Structure instance is immutable: %s" inst))

(defmethod set-key! $mutable-structure-instance-tag [inst k v]
  (dosync (alter (:value inst)
                 (fn [iv] (merge iv {k v})))))

(defn make [basis & inits] (make-structure-instance basis (plist->map inits)))

(defn instance-of? [x t] ((type-predicate t) x))

;;; ----------------------------------------------------------------------
;;; Domains
;;; ----------------------------------------------------------------------

(defn domain [& inits](apply make `(~<domain> ~@inits)))

;;; ----------------------------------------------------------------------
;;; Functions
;;; ----------------------------------------------------------------------

(defn function [dom] (make-function dom))

