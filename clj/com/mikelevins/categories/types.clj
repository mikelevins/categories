;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.clj
;;;; Project:       Categories
;;;; Purpose:       type representation
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(ns com.mikelevins.categories.types
  (:refer-clojure :exclude [type]))

(refer 'com.mikelevins.categories.utils)
(refer 'com.mikelevins.categories.structures)

;;; ======================================================================
;;; Type Tags
;;; ======================================================================

(def $primitive-type-tag ::<primitive-type>)
(def $synonym-type-tag ::<synonym-type>)
(def $category-type-tag ::<category-type>)

;;; ======================================================================
;;; Primitive types
;;; ======================================================================

(defn make-primitive-type [tag predicate]
  (with-meta {:tag tag :predicate predicate}
    {:type $primitive-type-tag}))

(defn primitive-type? [x] (= $primitive-type-tag (clojure.core/type x)))

(defmethod clojure.core/print-method $primitive-type-tag [o, #^java.io.Writer w]
  (.write w (format "#<primitive-type: %s>" (:tag o))))

;;; ======================================================================
;;; Synonym types
;;; ======================================================================

(defn make-synonym-type [original-type]
  (with-meta {:original-type original-type}
    {:type $synonym-type-tag}))

(defn synonym-type? [x](= $synonym-type-tag (clojure.core/type x)))

(defn original-type [x] (:original-type x))

(defmethod clojure.core/print-method $synonym-type-tag [o, #^java.io.Writer w]
  (.write w "#<a synonym for: ")
  (clojure.core/print-method (original-type o) w)
  (.write w ">"))

;;; ======================================================================
;;; Category types
;;; ======================================================================

;;; forward declaration
(defn type-predicate)

(defn make-category-predicate [types]
  (fn [x] (and (some (fn [t] ((type-predicate t) x)) types) true)))

(defn make-category-type [member-types]
  (with-meta {:member-types member-types
              :predicate (make-category-predicate member-types)}
    {:type $category-type-tag}))

(defn category-type? [x](= $category-type-tag (clojure.core/type x)))

(defmethod clojure.core/print-method $category-type-tag [o, #^java.io.Writer w]
  (.write w "#<category: ")
  (clojure.core/print-method (:member-types o) w)
  (.write w ">"))

;;; ======================================================================
;;; general type utils
;;; ======================================================================

(defn type-representation? [t]
  (or (class? t)
      (primitive-type? t)
      (structure-basis? t)
      (synonym-type? t)
      (category-type? t)))

(defn type-predicate [t]
  (cond
    (class? t) (fn [x] (= (class x) t))
    (primitive-type? t) (:predicate t)
    (synonym-type? t) (:predicate (original-type t))
    (category-type? t) (:predicate t)
    (structure-basis? t) (:predicate (meta t))
    :else (error "Not a type: " t)))

