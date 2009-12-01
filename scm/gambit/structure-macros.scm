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

(define (%parse-key-qualifiers quals)(utils:plist->alist quals))

(define (%parse-keyspec spec)
  (if (symbol? spec)
      `(list (quote ,spec) #f #f)
      (if (and (pair? spec)
               (symbol? (car spec)))
          (let* ((quals (%parse-key-qualifiers (cdr spec)))
                 (default (utils:get quals default: eq? #f))
                 (setter? (utils:get quals setter: eq? #f)))
            (cons 'list
                  (cons `(quote ,(car spec))
                        (list default setter?)))))))

(define (%parse-keyspecs specs)
  (map (lambda (s) (%parse-keyspec s))
       specs))

;;; (structure (included-structure1 ...) key1 ...) => #<a-structure-basis>
(define-macro (structure includes . keyspecs)
  (let ((keyspecs (%parse-keyspecs keyspecs)))
    `(structs:construct-structure-basis
      (structs:merge-basis-includes (list ,@includes) 
                                    (list ,@keyspecs)))))

