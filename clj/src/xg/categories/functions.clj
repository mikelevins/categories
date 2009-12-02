;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.clj
;;;; Project:       Categories
;;;; Purpose:       generic functions implementation
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(ns xg.categories.functions
  (:refer-clojure :exclude [type])
  (:require xg.categories.utils
            xg.categories.structures
            xg.categories.types
            xg.categories.domains))

(refer 'xg.categories.utils)
(refer 'xg.categories.structures)
(refer 'xg.categories.types)
(refer 'xg.categories.domains)

;;; ======================================================================
;;; Functions
;;; ======================================================================

(def $function-type-tag ::<function>)

(def <method>
     (construct-structure-basis 
      (into {} (map parse-keyspec '(:signature :method-function)))))



(defn method-signature [m] (:signature m))
(defn method-function [m] (:method-function m))
(defn make-method [sig mfun] (make-structure-instance <method> {:signature sig :method-function mfun}))

;;; ======================================================================
;;; Method tables
;;; ======================================================================

(def <method-table>
     (construct-structure-basis 
      (into {} (map parse-keyspec '(:entries)))))

(defn get-method-entries [m] (deref (:entries m)))

(defn make-method-table [] (make-structure-instance <method-table> {:entries (ref {})}))

;;; ======================================================================
;;; Function objects
;;; ======================================================================

(defn function-object [closure]
  (:function-object (meta closure)))

(def <function>
     (construct-structure-basis 
      (into {} (map parse-keyspec '(:domain :method-table)))))

(defn get-domain [f] (:domain f))
(defn get-method-table [f] (:method-table f))

(defn add-method! [f m]
  (let [fobj (function-object f)]
    (if fobj
      (let [adder (get-method-adder (get-domain fobj))]
        (adder fobj m))
      (error "Can't get reflection data for function: %s" f))))

(defn remove-method! [f sig]
  (let [fobj (function-object f)]
    (if fobj
      (let [remover (get-method-remover (get-domain fobj))]
        (remover fobj sig))
      (error "Can't get reflection data for function: %s" f))))

(defn compute-applicable-methods [fobj args]
  (let [selector (get-method-selector (get-domain fobj))]
    (selector fobj args)))

(def next-method)
(def other-applicable-methods)

(defn apply-function [fobj args]
  (let [methods-obj (compute-applicable-methods fobj args)]
    (if methods-obj
      (let [effectivem (:effective-method methods-obj)
            nextm (:next-method methods-obj)
            otherms (:ordered-other-applicable-methods methods-obj)
            default-next (fn [& args] (error "No next method"))]
        (binding [next-method (if nextm
                                (method-function nextm)
                                default-next)
                  other-applicable-methods (if otherms
                                             (if (empty? otherms)
                                               (list default-next)
                                               (map method-function otherms))
                                             (list default-next))]
          (apply (method-function effectivem) args)))
      (error "No applicable method: " args))))

(defn make-function [dom]
  (let [fobj (make-structure-instance <function>
                                      {:domain dom
                                       :method-table (make-method-table)})
        metadata {:type $function-type-tag :function-object fobj}]
    (proxy [clojure.lang.IMeta clojure.lang.IFn] []
      (invoke [& args] (apply-function fobj args))
      (meta [] metadata))))

(defmethod clojure.core/print-method $function-type-tag [o, #^java.io.Writer w]
  (.write w (format "#<generic function %s>" (.hashCode o))))

