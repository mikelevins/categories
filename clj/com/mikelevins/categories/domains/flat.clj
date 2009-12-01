;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          flat.clj
;;;; Project:       Categories
;;;; Purpose:       the -flat-domain
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(ns com.mikelevins.categories.domains.flat
  (:refer-clojure :exclude [type]))

(refer 'com.mikelevins.categories.utils)
(refer 'com.mikelevins.categories.structures)
(refer 'com.mikelevins.categories.types)
(refer 'com.mikelevins.categories.domains)
(refer 'com.mikelevins.categories.functions)
(refer 'com.mikelevins.categories)

;;; ======================================================================
;;; -flat-
;;; ======================================================================

(defn flat-select-methods [fobj vals]
  (let [sig (map type vals)
        m (get (get-method-entries (get-method-table fobj))
               sig
               false)]
    (if m
      {:effective-method m :next-method false :ordered-other-applicable-methods false}
      false)))

(def -flat- (domain :method-selector flat-select-methods))

;;; ======================================================================
;;; conveniences
;;; ======================================================================

(defmacro method [params & body]
  (let [vars (map first params)
        quals (map second params)]
    `(make-method [~@quals] (fn [~@vars] ~@body))))
