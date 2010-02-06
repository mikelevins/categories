;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tagged-vectors.scm
;;;; Project:       Categories
;;;; Purpose:       utils for representing structured types
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(define (tags:tag x)(vector-ref x 0))

(define (tags:tagged-vector? x tag)
  (and (vector? x)
       (> (vector-length x) 0)
       (eqv? tag (tags:tag x))))

