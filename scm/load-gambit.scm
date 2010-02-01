;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load-gambit.scm
;;;; Project:       Categories
;;;; Purpose:       load Gambit-specific Categories
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

;;; modify if the Categories sources are at another pathname

(define $root "/Users/mikel/Valise/xg/repositories/categories/scm")

;;; ----------------------------------------------------------------------
;;; About
;;; ----------------------------------------------------------,------------

;;; ----------------------------------------------------------------------
(define $load-files
  '(;; infrastructure
    "/gambit/errors.scm"
    "/gambit/utils.scm"
    ;; Categories
    "/gambit/tagged-vectors.scm"
    "/gambit/structures.scm"
    "/gambit/structure-macros.scm"
    "/gambit/types.scm"
    "/gambit/domains.scm"
    "/gambit/functions.scm"
    "/gambit/categories.scm"
    "/gambit/flat-domain.scm"
    "/gambit/flat-domain-macros.scm"
    "/gambit/c3.scm"
    "/gambit/c3-domain.scm"
    "/gambit/c3-domain-macros.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-categories)
  (for-each (lambda (f)
              (let ((p (string-append $root f)))
                (load p)))
            $load-files))
