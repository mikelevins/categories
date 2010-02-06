;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          flat-domain--macros.scm
;;;; Project:       Categories
;;;; Purpose:       representation of types
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(define-macro (flat:method args . body) 
  (let ((ids (map car args))
        (quals (map cadr args)))
    `(make <method>
       'signature (list ,@quals)
       'method-function (lambda ,ids ,@body)
       'domain =flat=)))
