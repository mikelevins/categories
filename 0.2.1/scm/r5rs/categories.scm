;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          categories.scm
;;;; Project:       Categories
;;;; Purpose:       The public API
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

;;;; ABOUT
;;;; The Categories surface syntax.
;;;; Many forms in this file are defined in the infrastructure, and are
;;;; redefined here purely for reference purposes.

;;; ----------------------------------------------------------------------
;;; types
;;; ----------------------------------------------------------------------

;;; the primitive types 
;;; ----------------------------------------------------------------------

;;; numbers
(define <integer> (types:make-primitive-type '<integer> integer?))
(define <rational> (types:make-primitive-type '<rational> rational?))
(define <real> (types:make-primitive-type '<real> real?))
(define <complex> (types:make-primitive-type '<complex> complex?))
(define <number> (types:make-primitive-type '<number> number?))

;;; booleans
(define <boolean> (types:make-primitive-type '<boolean> boolean?))
(define <true> (types:make-primitive-type '<true> (lambda (x) (eqv? x #t))))
(define <false> (types:make-primitive-type '<false> (lambda (x) (eqv? x #f))))

;;; pairs and lists
(define <empty-list> (types:make-primitive-type '<empty-list> null?))
(define <pair> (types:make-primitive-type '<pair> pair?))
(define <list> (types:make-primitive-type '<list> list?))

;;; symbols
(define <symbol> (types:make-primitive-type '<symbol> symbol?))

;;; characters
(define <character> (types:make-primitive-type '<character> char?))

;;; strings
(define <string> (types:make-primitive-type '<string> string?))

;;; vectors
(define <vector> (types:make-primitive-type '<vector> vector?))

;;; procedures
(define <procedure> (types:make-primitive-type '<procedure> procedure?))

;;; ports
(define <port> (types:make-primitive-type '<port> (lambda (x) (or (input-port? x)(output-port? x)))))
(define <input-port> (types:make-primitive-type '<input-port> input-port?))
(define <output-port> (types:make-primitive-type '<output-port> output-port?))

;;; end-of-file
(define <end-of-file> (types:make-primitive-type '<end-of-file> eof-object?))

;;; structures 
;;; ----------------------------------------------------------------------

;;; (structure (included-structure1 ...) key1 ...) => #<a-structure-basis>
;;; keys: key | (key [default: val]? [setter: #t | #f]?)
(define-syntax structure
  (lambda (form)
    (syntax-case form ()
      ((_ (i0 ...) k0 ...)
       (syntax (structs:construct-structure-basis
                (structs:merge-basis-includes (list i0 ...) 
                                                (list (structs:parse-keyspec k0) ...))))))))

;;; (getter basis key) => #<a-getter-function>
;;; getter-function: (lambda (instance)...) => value
(define (getter struct-basis k)(structs:get-getter struct-basis k))

;;; (setter basis key) => #<a-setter-function>
;;; setter-function: (lambda (instance value)...) => instance'
(define (setter struct-basis k)(structs:get-setter struct-basis k))

;;; (get-key instance key) => value
(define (get-key s k) ((getter (structs:structure-basis s) k) s))

;;; (set-key! instance key value) => instance'
(define (set-key! s k v) 
  (let ((s! (setter (structs:structure-basis s) k)))
    (if s!
        (begin
          (s! s v)
          s)
        (error "Can't assign to a read-only key: " k))))

;;; synonyms
;;; ----------------------------------------------------------------------

;;; (synonym type) => #<a-synonym-type>
(define synonym types:make-synonym-type)

;;; (synonym? type) => #t | #f
(define synonym? types:synonym-type?)

;;; (original-type synonym-type) => #<a-type>
(define original-type types:original-type)

;;; categories
;;; ----------------------------------------------------------------------

;;; (category type1 ...) => #<a-category-type>
(define (category . types) (types:make-category-type types))

;;; (category? type) => #t | #f
(define category? types:category-type?)

;;; (member-types category-type) => (type1 ...)
(define (member-types tp) 
  (if (category? tp)
      (vector-ref tp 1)
      (error "Not a category: " tp)))

;;; type utils
;;; ----------------------------------------------------------------------

;;; (instance-of? value type) => #t | #f
(define (instance-of? val tp) ((types:type-predicate tp) val))

;;; ----------------------------------------------------------------------
;;; functions
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

;;; ----------------------------------------------------------------------
;;; domains
;;; ----------------------------------------------------------------------

;;; (domain 'domain-data data 'method-adder adder-fn 
;;;         'method-remover remover-fn 'method-selector selector-fn)
;;;  => #<a-domain>
;;; adder-fn: (lambda (function method) e1...) => function'
;;; remover-fn: (lambda (function method-signature) e1...) => function'
;;; selector-fn: (lambda (function values) e1...) => effective-method next-method ordered-other-applicable-methods
(define (domain . initargs)(apply make `(,<domain> ,@initargs)))
