;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          flat-domain.scm
;;;; Project:       Categories
;;;; Purpose:       The -flat- domain
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; auxiliary functions
;;; ----------------------------------------------------------------------

(define (flat:find-method fun vals)
  (let* ((vsig (map type vals))
         (mtable (get-method-table fun))
         (entries (fun:get-method-entries mtable)))
    (utils:get entries vsig equal? #f)))

;;; ----------------------------------------------------------------------
;;; imoplementation of the domain API
;;; ----------------------------------------------------------------------

(define (flat:select-methods fun vals)
  (let ((meth (flat:find-method fun vals)))
    (if meth
        (values meth #f #f)
        (values #f #f #f))))

(define -flat- (domain 'method-selector flat:select-methods))



