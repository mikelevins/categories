;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          version.scm
;;;; Project:       Categories
;;;; Purpose:       the Categories object system
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************


(define $categories-vm-version (vector 0 4 0))

(define (categories-major-version)
  (vector-ref $categories-vm-version 0))

(define (categories-minor-version)
  (vector-ref $categories-vm-version 1))

(define (categories-update-version)
  (vector-ref $categories-vm-version 2))

(define (categories-version-string)
  (string-append (number->string (categories-major-version))
                 "."
                 (number->string (categories-minor-version))
                 "."
                 (number->string (categories-update-version))))

