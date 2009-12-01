;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.clj
;;;; Project:       Categories
;;;; Purpose:       general utilitis
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(ns com.mikelevins.categories.utils)

(defn str-or-nil [x] (if (nil? x) "nil" (str x)))

(defn error [msg & args] (throw (Exception. (apply (partial format msg) (map str-or-nil args)))))

(defn plist->map [plist]
  (loop [pl plist
         acc {}]
    (if (empty? pl)
      acc
      (let [k (first pl)
            more (rest pl)]
        (if (contains? acc k)
          (error "Duplicate key in property-list: %s" k)
          (if (empty? more)
            (error "Malformed property-list: %s" plist)
            (let [v (first more)
                  remainder (rest more)]
              (recur remainder (merge acc {k v})))))))))

(defn member [item seq](some (fn [s] (= item s)) seq))

