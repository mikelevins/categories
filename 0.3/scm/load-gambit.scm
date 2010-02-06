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

(define $root "/Users/mikel/Valise/xg/repositories/categories/0.3/scm")



;;; ----------------------------------------------------------------------
;;; About
;;; ----------------------------------------------------------,------------

;;; ----------------------------------------------------------------------
(define $load-files
  '(;; infrastructure
    ;;"/gambit/utils.scm"
    ;; Categories
    ;;"/gambit/tagged-vectors.scm"
    ;;"/gambit/structures.scm"
    ;;"/gambit/structure-macros.scm"
    "/gambit/types.scm"
    "/gambit/protocols.scm"
    ;;"/gambit/domains.scm"
    ;;"/gambit/functions.scm"
    ;;;"/gambit/categories.scm" -- reference only; not loaded
    ;;"/gambit/flat-domain.scm"
    ;;"/gambit/flat-domain-macros.scm"
    ;;"/gambit/c3.scm"
    ;;"/gambit/c3-domain.scm"
    ;;"/gambit/c3-domain-macros.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-categories)
  (let ((blackhole-loader (lambda () (load "~~/lib/modules/build")))
        (error-handler (lambda (err)
                         (newline)
                         (if (and (os-exception? err)
                                  (= (os-exception-code err) -507510774))
                             (begin
                               (display "attempted to reload a library; skipping...")
                               (newline))
                             (begin
                               (display err)
                               (display " ")
                               (display (os-exception-code err))))
                         (newline))))
    (with-exception-catcher error-handler blackhole-loader))
  (for-each (lambda (f)
              (let ((p (string-append $root f)))
                (load p)))
            $load-files))
