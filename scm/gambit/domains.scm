;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          domains.scm
;;;; Project:       Categories
;;;; Purpose:       implementation of domains
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(include "structure-macros.scm")

(define (default-method-adder)
  (lambda (fun meth)
    (let* ((mtable (get-method-table fun)))
      (fun:add-method-entry! mtable meth))))

(define (default-method-remover)
  (lambda (fun sig)
    (let* ((mtable (get-method-table fun)))
      (fun:remove-method-entry! mtable sig))))

(define <domain> 
  (structure () 
             domain-data
             (method-adder default: (default-method-adder))
             (method-remover default: (default-method-remover))
             method-selector))

;;; domain-data can be any data structure (or may be ignored)
(define dom:get-domain-data (getter <domain> 'domain-data))
(define dom:set-domain-data! (setter <domain> 'domain-data))

;;; method-adder: (lambda (function method)...) => function'
;;; the method-adder is responsible for determining whether it is safe to add
;;; a given method, and signaling an error if not.
(define dom:get-method-adder (getter <domain> 'method-adder))

;;; method-remover: (lambda (function signature)...) => function'
(define dom:get-method-remover (getter <domain> 'method-remover))

;;; method-selector: (lambda (function param-vals)...) => effectivem nextm otherms
;;; the method selector is responsible for determining which, if any, methods are applicable to the
;;; given arguments. If the domain supports dispatching on supertypes, then it must order all applicable
;;; methods and return the most specific as effectivem, the next most specific as nextm, and
;;; all others in order from most to least specific as otherms. If the domain does not support
;;; dispatching on superrtypes, it should return the applicable method as effectivem, and 
;;; return #f for nextm and otherms. If it is not possible to compute a total ordering of
;;; applicable methods, then the selector must signal a dispatch error.
(define dom:get-method-selector (getter <domain> 'method-selector))

(define (domain . initargs)(apply make `(,<domain> ,@initargs)))
