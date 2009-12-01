;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.scm
;;;; Project:       Categories
;;;; Purpose:       implementation of generic functions
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(include "structure-macros.scm")

;;; ----------------------------------------------------------------------
;;; function registry
;;; ----------------------------------------------------------------------
;;; in this implementation, generic functions are represented by
;;; closures that provide the entry point to the dispatching
;;; framework. there's no striaghtforward way in R5RS to look inside a
;;; closure, nor to create a new kind of callable object, so in order
;;; to enable things like add-method! to work, we keep a grand
;;; galactic table of closures mapped to their <function>
;;; objects. That way, when you call (add-method! some-fun
;;; some-method), Categories has a way to get hold of the method table
;;; and modify it.

(define fun:*functions* (make-table test: eq?))

(define (fun:register-function closure function-object)
  (table-set! fun:*functions* closure function-object))

(define (fun:function-object closure)(table-ref fun:*functions* closure))

;;; ----------------------------------------------------------------------
;;; representing methods
;;; ----------------------------------------------------------------------

(define <method> (structure () signature method-function))

(define fun:method-signature (getter <method> 'signature))
(define fun:method-function (getter <method> 'method-function))

;;; ----------------------------------------------------------------------
;;; method-tables
;;; ----------------------------------------------------------------------

;;; ABOUT

;;; A method table stores all the defined methods for a given
;;; function.  The methods are instances of <method>, and are stored
;;; in the entries field, which is an alist mapping signatures to
;;; method instances.  fun:select-applicable-methods uses the
;;; matcher function of the appropriate domain to collect those
;;; methods that match a given set of inputs.

(define <method-table> (structure () (entries default: '() setter: #t)))

(define (fun:make-method-table)(make <method-table>))

(define fun:get-method-entries (getter <method-table> 'entries))
(define fun:set-method-entries! (setter <method-table> 'entries))

(define (fun:add-method-entry! mtable meth)
  (let* ((entries (fun:get-method-entries mtable))
         (sig (fun:method-signature meth))
         (already-entry (utils:get-entry entries sig equal? #f)))
    (if already-entry
        (set-cdr! already-entry meth)
        (fun:set-method-entries! mtable (cons (cons sig meth) entries)))))

(define (fun:remove-method-entry! mtable sig)
  (let* ((entries (fun:get-method-entries mtable)))
    (fun:set-method-entries! mtable
                             (utils:filter (lambda (entry) 
                                             (not (equal? sig (car entry))))
                                           entries))))

;;; ----------------------------------------------------------------------
;;; functions
;;; ----------------------------------------------------------------------

(define next-method #f)
(define other-applicable-methods #f)

(define <function> (structure () domain method-table))

(define get-domain (getter <function> 'domain))
(define get-method-table (getter <function> 'method-table))

(define (fun:add-method! fun meth)
  (let ((function-object (fun:function-object fun)))
    (if function-object
        (let ((adder (dom:get-method-adder (get-domain function-object))))
          (adder function-object meth))
        (errs:error "Can't get reflection data for function: " fun))))

(define (fun:remove-method! fun sig)
  (let ((function-object (fun:function-object fun)))
    (if function-object
        (let ((remover (dom:get-method-remover (get-domain function-object))))
          (remover function-object sig))
        (errs:error "Can't get reflection data for function: " fun))))

(define (fun:compute-applicable-methods fun args)
  (let ((selector (dom:get-method-selector (get-domain fun))))
    (selector fun args)))

(define (fun:apply-function fun args)
  (call-with-values (lambda () (fun:compute-applicable-methods fun args))
    (lambda (effectivem nextm otherms)
      (if effectivem
          (let ((saved-next-method next-method)
                (saved-applicable-methods other-applicable-methods))
            (dynamic-wind 
                (lambda () 
                  (set! next-method
                        (if nextm
                            (fun:method-function nextm)
                            (lambda args (error "No next method"))))
                  (set! other-applicable-methods 
                        (if (and (list? otherms)
                                 (not (null? otherms)))
                            (map fun:method-function otherms))))
                (lambda () (apply (fun:method-function effectivem) args))
                (lambda ()
                  (set! next-method saved-next-method)
                  (set! other-applicable-methods saved-applicable-methods))))
          (errs:error "No applicable method:" fun args)))))

(define (fun:make-function dom)
  (let* ((function-object (make <function>
                            'domain dom 'method-table (fun:make-method-table)))
         (gf (lambda args (fun:apply-function function-object args))))
    (fun:register-function gf function-object)
    gf))


;;; ----------------------------------------------------------------------
;;; Fucntion API
;;; ----------------------------------------------------------------------

;;; (make-function domain) => #<a-function>
(define (function dom)(fun:make-function dom))

;;; (add-method! function method) => function'
(define (add-method! fun method)(fun:add-method! fun method))

;;; (remove-method! function method-signature) => function'
(define (remove-method! fun sig)(cat-fun:remove-method! fun sig))

;;; (get-domain function) => #<a-domain>
(define get-domain (getter <function> 'domain))

;;; (get-method-table function) => #<a-method-table>
(define get-method-table (getter <function> 'method-table))

