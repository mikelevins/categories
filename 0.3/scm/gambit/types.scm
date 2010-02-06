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
;;; Tagged Vectors -- for representing types
;;; ======================================================================

(define (types:%tag x)(vector-ref x 0))
(define (types:type-tag x)(vector-ref x 0))
(define (types:type-predicate t)(vector-ref t 2))

(define (types:tagged-vector? x tag)
  (and (vector? x)
       (> (vector-length x) 0)
       (eqv? tag (types:%tag x))))

;;; ======================================================================
;;; Type Tags
;;; ======================================================================

(define types:$primitive-type-tag '<primitive-type>)
(define types:$cell-type-tag '<cell-type>)
(define types:$sequence-type-tag '<sequence-type>)
(define types:$record-type-tag '<record-type>)
(define types:$map-type-tag '<map-type>)
(define types:$category-type-tag '<category-type>)

;;; ======================================================================
;;; Primitive types
;;; ======================================================================

(define (types:make-primitive-type tag pred)
  (vector types:$primitive-type-tag tag pred))

(define (types:primitive-type? x)
  (types:tagged-vector? x types:$primitive-type-tag))

;;; ----------------------------------------------------------------------

;; numbers
(define <fixnum> (types:make-primitive-type '<fixnum> fixnum?))
(define <flonum> (types:make-primitive-type '<flonum> flonum?))
(define <bignum> (types:make-primitive-type '<bignum> (lambda (x) (and (##subtyped? x) (= (##subtype x) 31)))))
(define <integer> (types:make-primitive-type '<integer> integer?))
(define <real> (types:make-primitive-type '<real> real?))
(define <complex> (types:make-primitive-type '<complex> complex?))
(define <rational> (types:make-primitive-type '<rational> rational?))
(define <number> (types:make-primitive-type '<number> number?))

;; booleans
(define <true> (types:make-primitive-type '<true> (lambda (val) (eqv? val #t))))
(define <false> (types:make-primitive-type '<false> (lambda (val) (eqv? val #f))))
(define <boolean> (types:make-primitive-type '<boolean> boolean?))

;; pairs and lists
(define <empty-list> (types:make-primitive-type '<empty-list> null?))
(define <list> (types:make-primitive-type '<list> list?))
(define <pair> (types:make-primitive-type '<pair> pair?))

;; symbols
(define <identifier> (types:make-primitive-type '<identifier> (lambda (x) (or (symbol? x)(keyword? x)))))
(define <symbol> (types:make-primitive-type '<symbol> symbol?))
(define <keyword> (types:make-primitive-type '<keyword> keyword?))

;; characters
(define <character> (types:make-primitive-type '<character> char?))

;; strings
(define <string> (types:make-primitive-type '<string> string?))

;; vectors
(define <vector> (types:make-primitive-type '<vector> vector?))
(define <s8vector> (types:make-primitive-type '<s8vector> s8vector?))
(define <u8vector> (types:make-primitive-type '<u8vector> u8vector?))
(define <s16vector> (types:make-primitive-type '<s16vector> s16vector?))
(define <u16vector> (types:make-primitive-type '<u16vector> u16vector?))
(define <s32vector> (types:make-primitive-type '<s32vector> s32vector?))
(define <u32vector> (types:make-primitive-type '<u32vector> u32vector?))
(define <f32vector> (types:make-primitive-type '<f32vector> f32vector?))
(define <s64vector> (types:make-primitive-type '<s64vector> s64vector?))
(define <u64vector> (types:make-primitive-type '<u64vector> u64vector?))
(define <f64vector> (types:make-primitive-type '<f64vector> f64vector?))

;; procedures
(define <callable> (types:make-primitive-type '<callable> (lambda (x)(or (procedure? x)(continuation? x)))))
(define <procedure> (types:make-primitive-type '<procedure> procedure?))
(define <continuation> (types:make-primitive-type '<continuation> continuation?))

;; ports
(define <input-port> (types:make-primitive-type '<input-port> input-port?))
(define <output-port> (types:make-primitive-type '<output-port> output-port?))
(define <port> (types:make-primitive-type '<port> (lambda (x) (or (input-port? x)(outpput-port? x)))))

;; eof
(define <end-of-file> (types:make-primitive-type '<end-of-file> eof-object?))

;; gambit records
(define <gambit-record> (types:make-primitive-type '<gambit-record>
                                                   (lambda (val) (and (##subtyped? val) (= (##subtype val) 4)))))

;; foreign types
(define <foreign> (types:make-primitive-type '<foreign> foreign?))

;; unidentified gambit "special value"
(define <gambit-special-value> (types:make-primitive-type '<gambit-special-value> ##special?))

;;; ======================================================================
;;; Cell types
;;; ======================================================================

(define (types:make-cell-type tag pred)
  (vector types:$cell-type-tag tag pred))

(define (types:cell-type? x)
  (types:tagged-vector? x types:$cell-type-tag))

;;; ----------------------------------------------------------------------

(define <cell> (types:make-cell-type '<cell> box?))

;;; ======================================================================
;;; Sequence types
;;; ======================================================================

(define (types:make-sequence-type tag pred)
  (vector types:$sequence-type-tag tag pred))

(define (types:sequence-type? x)
  (types:tagged-vector? x types:$sequence-type-tag))

;;; ----------------------------------------------------------------------

;;; ======================================================================
;;; Record types
;;; ======================================================================

(define (types:make-record-type tag pred)
  (vector types:$record-type-tag tag pred))

(define (types:record-type? x)
  (types:tagged-vector? x types:$record-type-tag))

;;; ----------------------------------------------------------------------

;;; ======================================================================
;;; Map types
;;; ======================================================================

(define (types:make-map-type tag pred)
  (vector types:$map-type-tag tag pred))

(define (types:map-type? x)
  (types:tagged-vector? x types:$map-type-tag))

;;; ----------------------------------------------------------------------

;;; ======================================================================
;;; Category types
;;; ======================================================================

(define (types:make-category-type tag pred)
  (vector types:$category-type-tag tag pred))

(define (types:category-type? x)
  (types:tagged-vector? x types:$category-type-tag))

;;; ----------------------------------------------------------------------

;;; ======================================================================
;;; General type utils
;;; ======================================================================

(define (type? t) 
  (and (vector? t)
       (> (vector-length x) 0)
       (memq (vector-ref t 0)
             `(,types:$primitive-type-tag
               ,types:$cell-type-tag
               ,types:$sequence-type-tag
               ,types:$record-type-tag
               ,types:$map-type-tag
               ,types:$category-type-tag))))

(define (type val)
  (cond
   ;; Categories types
   ((types:primitive-type? val) <primitive-type>)
   ((types:cell-type? val) <cell-type>)
   ((types:sequence-type? val) <sequence-type>)
   ((types:record-type? val) <record-type>)
   ((types:map-type? val) <map-type>)
   ((types:category-type? val) <category-type>)
   ;; numbers
   ((fixnum? val) <fixnum>)
   ((flonum? val) <flonum>)
   ((and (##subtyped? val) (= (##subtype val) 31)) <bignum>)
   ((integer? val) <integer>)
   ((real? val) <real>)
   ((complex? val) <complex>)
   ((rational? val) <rational>)
   ((number? val) <number>)
   ;; booleans
   ((eqv? val #t) <true>)
   ((eqv? val #f) <false>)
   ((boolean? val) <boolean>)
   ;; pairs and lists
   ((null? val) <empty-list>)
   ((list? val) <list>)
   ((pair? val) <pair>)
   ;; symbols
   ((symbol? val) <symbol>)
   ((keyword? val) <keyword>)
   ;; characters
   ((char? val) <character>)
   ;; strings
   ((string? val) <string>)
   ;; vectors
   ((vector? val) <vector>)
   ((s8vector? val) <s8vector>)
   ((u8vector? val) <u8vector>)
   ((s16vector? val) <s16vector>)
   ((u16vector? val) <u16vector>)
   ((s32vector? val) <s32vector>)
   ((u32vector? val) <u32vector>)
   ((f32vector? val) <f32vector>)
   ((s64vector? val) <s64vector>)
   ((u64vector? val) <u64vector>)
   ((f64vector? val) <f64vector>)
   ;; procedures
   ((procedure? val) <procedure>)
   ((continuation? val) <continuation>)
   ;; ports
   ((input-port? val) <input-port>)
   ((output-port? val) <output-port>)
   ;; eof
   ((eof-object? val) <end-of-file>)
   ;; gambit records
   ((and (##subtyped? val) (= (##subtype val) 4)) <record>)
   ;; foreign types
   ((foreign? val) <foreign>)
   ;; unidentified gambit "special value"
   ((##special? val) <gambit-special-value>)
   ;; default
   (else (error "Unknown value type" val))))
