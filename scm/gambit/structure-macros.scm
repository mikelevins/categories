;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          structure-macros.scm
;;;; Project:       Categories
;;;; Purpose:       macros for working with structures
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

;;; (structure (included-structure1 ...) key1 ...) => #<a-structure-basis>
(define-macro (structure includes . keyspecs)
  (let ((keyspecs (%parse-keyspecs keyspecs)))
    `(structs:construct-structure-basis
      (structs:merge-basis-includes (list ,@includes) 
                                    (list ,@keyspecs)))))

