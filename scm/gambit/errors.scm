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
  (apply error `(,message ,@args)))

