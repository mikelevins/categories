;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          categories.scm
;;;; Project:       Categories
;;;; Version:       0.3
;;;; Purpose:       The public API
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;;; ABOUT
;;; ======================================================================
;;;; The Categories surface syntax.

;;; ======================================================================
;;; Changes
;;; ======================================================================
;;; 0.3 
;;; caching implemented for =c3= domain to drastically improve efficiency
;;; - naming convention for domains changed to be friendlier to R6RS;
;;;   -flat- changed to =flat=
;;;   -c3- changed to =c3=
;;; 0.2
;;; - added implementations in R5RS and Clojure
;;; - added -C3- domain
;;; 0.1
;;; - initial implementation

;;; ======================================================================
;;; Types
;;; ======================================================================

;; Categories types
(define <structure> (types:make-primitive-type '<structure> structs:structure-basis?))
(define <primitive-type> (types:make-primitive-type '<primitive-type> types:primitive-type?))
(define <synonym> (types:make-primitive-type '<synonym> types:synonym-type?))
(define <category> (types:make-primitive-type '<category> types:category-type?))
(define <type> (types:make-primitive-type '<type> type?))

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
(define <procedure> (types:make-primitive-type '<procedure> procedure?))
(define <continuation> (types:make-primitive-type '<continuation> continuation?))

;; ports
(define <input-port> (types:make-primitive-type '<input-port> input-port?))
(define <output-port> (types:make-primitive-type '<output-port> output-port?))

;; eof
(define <end-of-file> (types:make-primitive-type '<end-of-file> eof-object?))

;; gambit records
(define <record> (types:make-primitive-type '<record> (lambda (val) (and (##subtyped? val) (= (##subtype val) 4)))))

;; foreign types
(define <foreign> (types:make-primitive-type '<foreign> foreign?))

;; unidentified gambit "special value"
(define <gambit-special-value> (types:make-primitive-type '<gambit-special-value> ##special?))

;;; ----------------------------------------------------------------------
;;; Structures
;;; ----------------------------------------------------------------------

;;; (structure (included-structure1 ...) key1 ...) => #<a-structure-basis>
(define-macro (structure includes . keyspecs)
  (let ((keyspecs (%parse-keyspecs keyspecs)))
    `(structs:construct-structure-basis
      (structs:merge-basis-includes (list ,@includes) 
                                    (list ,@keyspecs)))))

(define (make basis . inits)
  (structs:init-structure-instance (structs:construct-structure-instance basis)
                                   inits))

;;; (getter basis key) => #<a-getter-function>
;;; getter-function: (lambda (instance)...) => value
(define (getter struct-basis k)(structs:get-getter struct-basis k))

;;; (setter basis key) => #<a-setter-function>
;;; setter-function: (lambda (instance value)...) => instance'
(define (setter struct-basis k)(structs:get-setter struct-basis k))

;;; (get-key instance key) => value
(define (get-key s k) ((getter (structs:instance-basis s) k) s))

;;; (set-key! instance key value) => instance'
(define (set-key! s k v) 
  ((setter (structs:instance-basis s) k) s v))

;;; ----------------------------------------------------------------------
;;; general type utils
;;; ----------------------------------------------------------------------

(define (type? t) 
  (or (types:primitive-type? t)
      (types:synonym-type? t)
      (types:category-type? t)
      (structs:structure-basis? t)))

(define (type val)
  (cond
   ;; Categories types
   ((structs:structure-basis? val) <structure>)
   ((structs:structure-instance? val) (%structure-instance-basis val))
   ((types:primitive-type? val) <primitive-type>)
   ((types:synonym-type? val) <synonym-type>)
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

;;; ----------------------------------------------------------------------
;;; Other Categories types
;;; ----------------------------------------------------------------------

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

;;; ======================================================================
;;; Domains
;;; ======================================================================

(define (default-method-adder)
  (lambda (fun meth)
    (let* ((mtable (get-method-table fun)))
      (fun:add-method-entry! mtable meth))))

(define (default-method-remover)
  (lambda (fun meth)
    (let* ((mtable (get-method-table fun)))
      (fun:remove-method-entry! mtable sig))))

(define <domain> 
  (structure () 
             domain-data
             (method-adder default: (default-method-adder))
             (method-remover default: (default-method-remover))
             method-selector))

(define (domain . initargs)(apply make `(,<domain> ,@initargs)))

;;; ======================================================================
;;; Functions
;;; ======================================================================

(define <method> (structure () signature method-function))

(define next-method #f)
(define other-applicable-methods #f)

(define <function> (structure () domain method-table))

(define get-domain (getter <function> 'domain))
(define get-method-table (getter <function> 'method-table))

