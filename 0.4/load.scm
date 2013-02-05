;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       Categories
;;;; Purpose:       load Categories in Gambit
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2013 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

;;; modify if the Categories sources are at another pathname

(define $root "/Users/mikel/Workshop/categories/0.4")

(define $load-files
  '("/src/version.scm"
    "/lib/Sort.scm"
    "/lib/uuid.scm"
    "/src/utils.scm"
    "/src/protocol.scm"
    "/src/c3.scm"
    "/src/function.scm"
    "/src/type.scm"
    "/src/protocol-boxes.scm"
    "/src/protocol-constructing.scm"
    "/src/protocol-converting.scm"
    "/src/protocol-functions.scm"
    "/src/protocol-ordering.scm"
    "/src/protocol-pairs.scm"
    "/src/protocol-mutable-pairs.scm"
    "/src/protocol-sequences.scm"
    "/src/protocol-mutable-sequences.scm"
    "/src/protocol-sets.scm"
    "/src/protocol-streams.scm"
    "/src/protocol-tables.scm"
    "/src/protocol-mutable-tables.scm"
    "/src/protocol-text.scm"))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-categories)
  (for-each (lambda (f)
              (let ((p (string-append $root f)))
                (load p)))
            $load-files))

;;; (load-categories)
