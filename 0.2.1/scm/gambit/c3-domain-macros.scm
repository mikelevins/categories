;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          c3-domain--macros.scm
;;;; Project:       Categories
;;;; Purpose:       macros for use with the c3 domain
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(define-macro (c3:method args . body) 
  (let ((ids (map car args))
        (quals (map cadr args)))
    `(make <method>
       'signature (list ,@quals)
       'method-function (lambda ,ids ,@body)
       'domain =c3=)))

