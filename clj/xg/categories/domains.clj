;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          domains.clj
;;;; Project:       Categories
;;;; Purpose:       domains implementation
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(ns xg.categories.domains
  (:refer-clojure :exclude [type]))

(refer 'xg.categories.utils)
(refer 'xg.categories.structures)
(refer 'xg.categories.types)

;;; ======================================================================
;;; Domains
;;; ======================================================================

(defn default-method-adder []
  (fn [fun meth] 
    (let [mtable (:method-table fun)]
      (dosync 
       (alter (:entries mtable)
              (fn [mt] (merge mt {(:signature meth) meth})))))))

(defn default-method-remover []
  (fn [fun sig]
    (let [mtable (:method-table fun)]
      (dosync 
       (alter (:entries mtable)
              (fn [mt] (dissoc mt sig)))))))

(def <domain>
     (construct-structure-basis 
      (into {}
            (map parse-keyspec `((:domain-data :default ~nil :setter ~true)
                                 (:method-adder :default ~(default-method-adder) :setter ~false)
                                 (:method-remover :default ~(default-method-remover) :setter ~false)
                                 (:method-selector :default ~false :setter ~false))))))


(defn get-domain-data [x] (deref (:domain-data (structure-map x))))
(defn set-domain-data! [x d] 
  (update-structure-map! x d)
  x)

(defn get-method-adder [x] (:method-adder (structure-map x)))
(defn get-method-remover [x] (:method-remover (structure-map x)))
(defn get-method-selector [x] (:method-selector (structure-map x)))
