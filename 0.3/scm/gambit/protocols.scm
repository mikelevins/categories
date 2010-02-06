;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocols.scm
;;;; Project:       Categories
;;;; Purpose:       representation of protocols
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2010 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; Protocol objects
;;; ======================================================================

(define (proto:make-protocol-object tag functions)
  (vector proto:$protocol-tag tag (make-table)))

(define (proto:protocol-object? x)
  (and (vector? x)
       (> (vector-length x) 2)
       (eqv? proto:$protocol-tag (vector-ref x 0))))

;;; ----------------------------------------------------------------------

