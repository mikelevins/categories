;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.scm
;;;; Project:       Categories
;;;; Purpose:       representation of types
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; Type Tags
;;; ======================================================================

(define types:$primitive-type-tag '<primitive-type>)
(define types:$synonym-type-tag '<synonym-type>)
(define types:$category-type-tag '<category-type>)

;;; ======================================================================
;;; Primitive types
;;; ======================================================================

(define (types:make-primitive-type tag pred)
  (vector types:$primitive-type-tag tag pred))

(define (types:primitive-type? x)
  (tags:tagged-vector? x types:$primitive-type-tag))

;;; ======================================================================
;;; Synonym types
;;; ======================================================================

(define (types:make-synonym-type original-type)
  (vector 'categories:synonym-type original-type))

(define (types:synonym-type? x)
  (tags:tagged-vector? x types:$synonym-type-tag))

(define (types:original-type x)
  (if (types:synonym-type? x)
      (vector-ref x 1)
      (categories:error "Not a type synonym: " x)))

;;; ----------------------------------------------------------------------
;;; categories support
;;; ----------------------------------------------------------------------

(define (types:make-category-predicate member-types)
  (lambda (v)
    (let loop ((tps member-types))
      (if (null? tps)
          #f
          (if (instance-of? v (car tps))
              #t
              (loop (cdr tps)))))))

(define (types:make-category-type member-types)
  (vector types:$category-type-tag 
          (utils:copy-tree member-types)
          (types:make-category-predicate member-types)))

(define (types:category-type? x)
  (tags:tagged-vector? x types:$category-type-tag))

;;; ----------------------------------------------------------------------
;;; general type utils
;;; ----------------------------------------------------------------------

(define (type? t) 
  (or (types:primitive-type? t)
      (types:synonym-type? t)
      (types:category-type? t)
      (structs:structure-basis? t)))

(define <structure> (types:make-primitive-type '<structure> structs:structure-basis?))
(define <primitive-type> (types:make-primitive-type '<primitive-type> types:primitive-type?))
(define <synonym> (types:make-primitive-type '<synonym> types:synonym-type?))
(define <category> (types:make-primitive-type '<category> types:category-type?))
(define <type> (types:make-primitive-type '<type> type?))

(define (types:type-predicate t)
  (cond
   ((types:primitive-type? t) (vector-ref t 2))
   ((types:synonym-type? t) (types:type-predicate (types:original-type t)))
   ((types:category-type? t) (vector-ref t 3))
   ((structs:structure-basis? t) (structs:basis-predicate t))
   (else (errs:error "Not a type: " t))))

(define (type val) 
  (cond
   ;; numbers
   ((integer? val) <integer>)
   ((rational? val) <rational>)
   ((real? val) <real>)
   ((complex? val) <complex>)
   ((number? val) <number>)
   ;; booleans
   ((eqv? val #t) <true>)
   ((eqv? val #f) <false>)
   ;; pairs and lists
   ((null? val) <empty-list>)
   ((list? val) <list>)
   ((pair? val) <pair>)
   ;; symbols
   ((symbol? val) <symbol>)
   ;; characters
   ((char? val) <character>)
   ;; strings
   ((string? val) <string>)
   ;; vectors
   ((vector? val) (cond
                   ((structs:structure-instance? val) (structs:structure-basis val))
                   ((structs:structure-basis? val) <structure>)
                   ((types:primitive-type? val) <primitive-type>)
                   ((types:synonym-type? val) <synonym>)
                   ((types:cateogory-type? val) <category>)
                   (else <vector>)))
   ;; procedures
   ((procedure? val) <procedure>)
   ;; ports
   ((input-port? val) <input-port>)
   ((output-port? val) <output-port>)
   ;; eof
   ((eof-object? val) <end-of-file>)
   (else (errs:error "Unknown value type" val))))

;;; the primitive types 
;;; ----------------------------------------------------------------------

;;; ======================================================================
;;; Public API
;;; ======================================================================

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
