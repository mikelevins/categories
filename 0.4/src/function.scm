;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          function.scm
;;;; Project:       Categories
;;;; Purpose:       the Categories object system
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define (make-function)
  (let ((methods '(metadata)))
    (lambda params params)))

(define (function-metadata fn)
  (cond ((##interp-procedure? fn)
         (##vector-ref (##interp-procedure-rte fn) 1))
        ((##closure? fn)
         (##closure-ref fn 1))
        (else
         (error "Not a function"))))
