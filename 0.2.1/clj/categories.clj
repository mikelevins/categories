;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          categories.clj
;;;; Project:       Categories
;;;; Purpose:       system definition for Categories
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(def $src-root "/Users/mikel/Valise/xg/repositories/categories/clj")
(def $categories-root (str $src-root "/src/xg"))
(load-file (str $src-root "/defsystem.clj"))

(require 'clojure.contrib.repl-utils)
(refer 'clojure.contrib.repl-utils)

;;; ----------------------------------------------------------------------
;;; system definitions
;;; ----------------------------------------------------------------------

(defsystem :categories
  :root $categories-root
  :files [
          "/categories/utils.clj"
          "/categories/structures.clj"
          "/categories/types.clj"
          "/categories/domains.clj"
          "/categories/functions.clj"
          "/categories.clj"
          "/categories/domains/flat.clj"
          "/categories/c3.clj"
          "/categories/domains/c3.clj"
          ])

(defn load-categories [](load-system :categories))