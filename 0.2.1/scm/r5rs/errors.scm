;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          errors.scm
;;;; Project:       Categories
;;;; Purpose:       error-reporting for r5rs
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(define (errs:error message . args)
  (display "ERROR: ")
  (display message)
  (for-each (lambda (arg) 
              (display " ")
              (write arg))
            args)
  (newline)
  (scheme-report-environment -1))

