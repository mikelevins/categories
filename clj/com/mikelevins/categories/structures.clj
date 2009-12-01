;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          structures.clj
;;;; Project:       Categories
;;;; Purpose:       representation of structures
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(ns com.mikelevins.categories.structures)

(refer 'com.mikelevins.categories.utils)

;;; ======================================================================
;;; Type Tags
;;; ======================================================================

(def $structure-basis-tag ::<structure-basis>)
(def $mutable-structure-basis-tag ::<mutable-structure-basis>)
(def $structure-instance-tag ::<structure-instance>)
(def $mutable-structure-instance-tag ::<mutable-structure-instance>)

(defn basis [x] (and (meta x)(:basis (meta x))))

;;; ======================================================================
;;; Structures
;;; ======================================================================
;;; like Clojure defstructs, but with support for additional constraints
;;; Note: Fairly likely this code will be reimplemented using Clojure
;;; Datatypes, once those become part of the trunk of Clojure

(defn keyspec-key [ks](key ks))
(defn keyspec-default [ks](first (val ks)))
(defn keyspec-mutable? [ks](second (val ks)))

(defn construct-structure-basis [keyspecs]
  (let [b (into {} keyspecs)
        mutable? (if (some keyspec-mutable? keyspecs)
                   true false)]
    (with-meta b
      {:type (if mutable? 
               $mutable-structure-basis-tag
               $structure-basis-tag)
       :predicate (fn [x] (= b (basis x)))})))

(defn structure-basis? [x] (= (clojure.core/type x) $structure-basis-tag))
(defn mutable-structure-basis? [b](= (clojure.core/type b) $mutable-structure-basis-tag))

(defn construct-structure-instance-map [keyspecs]
  (into {}
        (map (fn [e] [(key e)(keyspec-default e)])
             keyspecs)))

(defmulti construct-structure-instance clojure.core/type)

(defmethod construct-structure-instance :default [basis]
  (error "Not a valid structure-basis: %s" basis))

(defmethod construct-structure-instance $structure-basis-tag [basis]
  (with-meta (construct-structure-instance-map basis)
    {:type $structure-instance-tag :basis basis}))

(defmethod construct-structure-instance $mutable-structure-basis-tag [basis]
  (with-meta {:value (ref (construct-structure-instance-map basis))}
    {:type $mutable-structure-instance-tag  :basis basis}))

(defmulti structure-instance? clojure.core/type)

(defmethod structure-instance? :default [thing] false)
(defmethod structure-instance? $structure-instance-tag [thing] true)
(defmethod structure-instance? $mutable-structure-instance-tag [thing] true)

(defmulti mutable-structure-instance? clojure.core/type)

(defmethod mutable-structure-instance? :default [thing] false)
(defmethod mutable-structure-instance? $structure-instance-tag [thing] false)
(defmethod mutable-structure-instance? $mutable-structure-instance-tag [thing] true)

(defmulti structure-map clojure.core/type)

(defmethod structure-map :default [thing] false)
(defmethod structure-map $structure-instance-tag [thing] thing)
(defmethod structure-map $mutable-structure-instance-tag [thing] (deref (:value thing)))

(defmulti update-structure-map! (fn [s m] (clojure.core/type s)))

(defmethod update-structure-map! :default [x _] (error "Not a structure instance: %s" x))
(defmethod update-structure-map! $structure-instance-tag [x _] (error "Structure is immutable: %s" x))
(defmethod update-structure-map! $mutable-structure-instance-tag [x v] 
  (dosync (alter (:value x) (fn [xv] v))))

(defmulti init-structure-instance (fn [x y] (clojure.core/type x)))

(defmethod init-structure-instance :default [inst inits]
  (error "Not a structure instance: %s" inst))

(defn validate-structure-instance-inits [instance-keys inits]
  (let [extra-keys (clojure.set/difference (set (keys inits)) (set instance-keys))]
    (if (empty? extra-keys)
      inits
      (error "Invalid initialization keys in: %s" inits))))

(defmethod init-structure-instance $structure-instance-tag [inst inits] 
  (let [imeta (meta inst)
        inits (validate-structure-instance-inits (set (keys inst)) inits)]
    (with-meta (merge inst inits) imeta)))

(defmethod init-structure-instance $mutable-structure-instance-tag [inst inits] 
  (update-structure-map! inst 
                         (merge (structure-map inst) 
                                (validate-structure-instance-inits (keys (structure-map inst))
                                                                   inits)))
  inst)

(defn make-structure-instance [basis inits]
  (init-structure-instance (construct-structure-instance basis)
                           inits))

(defn merge-keys [base-keys new-keys]
  (let [bks (keys base-keys)
        nks (keys new-keys)]
    (if (some (fn [nk] (some (fn [bk] (= bk nk))
                             bks)) 
              nks)
      (error "Conflicting key definitions")
      (merge base-keys new-keys))))

(defn merge-basis-keys 
  ([bases] (merge-basis-keys bases {}))
  ([bases acc] (if (empty? bases)
                 acc
                 (merge-basis-keys (rest bases)
                                   (merge-keys acc (first bases))))))

(defmulti parse-keyspec class)
(defmethod parse-keyspec :default [x](error "Invalid keyspec: %s" x))
(defmethod parse-keyspec clojure.lang.Keyword [x] [x (list nil nil)])

(defmethod parse-keyspec clojure.lang.Sequential [x]
  (let [k (first x)
        opts (plist->map (rest x))]
    (if (isa? (class k) clojure.lang.Keyword)
      (let [default (:default opts)
            setter? (:setter opts)]
        [k (list default setter?)])
      (error "Invalid keyspec: %s" x))))

(defmethod clojure.core/print-method $structure-basis-tag [o, #^java.io.Writer w]
  (.write w "#<a <structure> basis with keys: ")
  (clojure.core/print-method (keys o) w)
  (.write w ">"))

(defmethod clojure.core/print-method $mutable-structure-basis-tag [o, #^java.io.Writer w]
  (.write w "#<a mutable <structure> basis with keys: ")
  (clojure.core/print-method (keys o) w)
  (.write w ">"))


(defmethod clojure.core/print-method $structure-instance-tag [o, #^java.io.Writer w]
  (.write w "#<a <structure> instance with keys: ")
  (clojure.core/print-method (keys o) w)
  (.write w ">"))

(defmethod clojure.core/print-method $mutable-structure-instance-tag [o, #^java.io.Writer w]
  (.write w "#<a mutable <structure> instance with keys: ")
  (clojure.core/print-method (keys (deref (:value o))) w)
  (.write w ">"))
