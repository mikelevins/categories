;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load-r5rs.scm
;;;; Project:       Categories
;;;; Purpose:       load portable r5rs Categories
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
;;; ----------------------------------------------------------------------

;;; ----------------------------------------------------------------------
(define $load-files
  '(;; infrastructure
    "/r5rs/errors.scm"
    "/r5rs/utils.scm"
    ;; Categories
    "/r5rs/tagged-vectors.scm"
    "/r5rs/structures.scm"
    "/r5rs/types.scm"
    "/r5rs/domains.scm"
    "/r5rs/functions.scm"
    "/r5rs/categories.scm"
    "/r5rs/flat-domain.scm"
    "/r5rs/c3.scm"
    "/r5rs/c3-domain.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-categories)
  (for-each (lambda (f)
              (let ((p (string-append $root f)))
                (load p)))
            $load-files))
