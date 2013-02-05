;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          type.scm
;;;; Project:       Categories
;;;; Purpose:       representation of Scheme and categories types
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "~~/lib/_gambit#.scm")

;;; ---------------------------------------------------------------------
;;; type identifiers
;;; ---------------------------------------------------------------------
;;; a type identifier is a symbol that uniquely identifies the
;;; type. Categories uses these symbols for efficient dispatch.

;;; ---------------------------------------------------------------------
;;; gambit built-in type and subtype tags
;;; ---------------------------------------------------------------------
;;; we use gambit's built-in type and subtype tags to efficiently
;;; discriminate the types of values.

;;; type tags

(define tags:$gambit-fixnum (macro-type-fixnum))
(define tags:$gambit-subtyped (macro-type-subtyped))
(define tags:$gambit-special (macro-type-special))
(define tags:$gambit-pair (##type '(a . b)))

;;; subtype tags

(define tags:$gambit-subtype-structure (macro-subtype-structure))

;;; =====================================================================
;;; typeids
;;; =====================================================================

(define (special-value-typeid thing)
  (cond
   ((null? thing) 'null)
   ((boolean? thing) 'boolean)
   ((char? thing) 'char)
   ((eqv? #!void thing) 'void)
   ((eqv? #!unbound thing) 'unbound)
   ((eqv? #!eof thing) 'eof)
   (else (error (string-append "Special value of unknown type: " (object->string thing))))))

(define $subtype-ids (make-table test: eqv?))

(define (def-subtype-id! id num)
  (table-set! $subtype-ids num id))

(define (get-subtype-id num)
  (table-ref $subtype-ids num #f))

;;; typeids for Gambit subtyped types
(def-subtype-id! 'vector (macro-subtype-vector))
(def-subtype-id! 'pair (macro-subtype-pair))
(def-subtype-id! 'ratnum (macro-subtype-ratnum))
(def-subtype-id! 'cpxnum (macro-subtype-cpxnum))
(def-subtype-id! 'structure (macro-subtype-structure))
(def-subtype-id! 'box (macro-subtype-boxvalues))
(def-subtype-id! 'meroon-object (macro-subtype-meroon))
(def-subtype-id! 'jazz-object (macro-subtype-jazz))

(def-subtype-id! 'symbol (macro-subtype-symbol))
(def-subtype-id! 'keyword (macro-subtype-keyword))
(def-subtype-id! 'frame (macro-subtype-frame))
(def-subtype-id! 'continuation (macro-subtype-continuation))
(def-subtype-id! 'promise (macro-subtype-promise))
(def-subtype-id! 'weak (macro-subtype-weak))
(def-subtype-id! 'procedure (macro-subtype-procedure))
(def-subtype-id! 'return (macro-subtype-return))

(def-subtype-id! 'foreign (macro-subtype-foreign))
(def-subtype-id! 'string (macro-subtype-string))
(def-subtype-id! 's8vector (##subtype (make-s8vector 1)))
(def-subtype-id! 'u8vector (##subtype (make-u8vector 1)))
(def-subtype-id! 's16vector (##subtype (make-s16vector 1)))
(def-subtype-id! 'u16vector (##subtype (make-u16vector 1)))
(def-subtype-id! 's32vector (##subtype (make-s32vector 1)))
(def-subtype-id! 'u32vector (##subtype (make-u32vector 1)))
(def-subtype-id! 'f32vector (##subtype (make-f32vector 1)))

(def-subtype-id! 's64vector (##subtype (make-s64vector 1)))
(def-subtype-id! 'u64vector (##subtype (make-u64vector 1)))
(def-subtype-id! 'f64vector (##subtype (make-f64vector 1)))

(define (structure-typeid thing)
  (##type-id (##structure-type thing)))

(define (subtyped-value-typeid thing)
  (if (##structure? thing)
      (structure-typeid thing)
      (get-subtype-id (##subtype thing))))

(define (typeid thing)
  (cond
   ((##fixnum? thing) 'fixnum)
   ((##flonum? thing) 'flonum)
   ((##bignum? thing) 'bignum)
   ((pair? thing) 'pair)
   ((##subtyped? thing)(subtyped-value-typeid thing))
   ((##special? thing)(special-value-typeid thing))
   (else (error (string-append "Value of unknown type: " (object->string thing))))))


;;; =====================================================================
;;; type objects
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; <schema>
;;; ---------------------------------------------------------------------
;;; a schema is a concrete type. all type objects are schemas; that is,
;;; they are instances of the <schema> type. 

(define-type schema id: 04C8E91C-AF6E-4AF0-9E8A-ED6258355017
  (name schema-name))

;;; ---------------------------------------------------------------------
;;; schema values
;;; ---------------------------------------------------------------------
;;; instances of <schema> that represent the various Gambit types

(define <schema> (make-schema '<schema>))
(define <class> (make-schema '<class>))
(define <singleton> (make-schema '<singleton>))
(define <function> (make-schema '<function>))
(define <protocol> (make-schema '<protocol>))

;;; ---------------------------------------------------------------------
;;; the defined schemas
;;; ---------------------------------------------------------------------

(define $typeid->type-object-table (make-table test: eq?))
(define $type-object->typeid-table (make-table test: eq?))

(define-macro (deftype type-name typeid)
  `(begin
     (define ,type-name (make-schema ',type-name))
     (table-set! $typeid->type-object-table ,typeid ,type-name)
     (table-set! $type-object->typeid-table ,type-name ,typeid)
     ,type-name))

(define (type-of val)
  (if (##structure? val)
      (##structure-type val)
      (table-ref $typeid->type-object-table (typeid val) #f)))

(define (%find-bignum)
  (let loop ((i 2))
    (if (##bignum? i)
        i
        (loop (* i i)))))

(define-type ignorable-structure a b c)

(deftype <bignum> 'bignum)
(deftype <boolean> 'boolean)
(deftype <box>  'box)
(deftype <char> 'char)
(deftype <continuation> 'continuation)
(deftype <cpxnum> 'cpxnum)
(deftype <eof> 'eof)
(deftype <fixnum> 'fixnum)
(deftype <flonum> 'flonum)
(deftype <frame> 'frame)
(deftype <foreign> 'foreign)
(deftype <jazz-object> 'jazz-object)
(deftype <keyword> 'keyword)
(deftype <meroon-object> 'meroon-object)
(deftype <null> 'null)
(deftype <pair> 'pair)
(deftype <procedure> 'procedure)
(deftype <promise> 'promise)
(deftype <ratnum> 'ratnum)
(deftype <return> 'return)
(deftype <s8vector> 's8vector)
(deftype <s16vector> 's16vector)
(deftype <s32vector> 's32vector)
(deftype <s64vector> 's64vector)
(deftype <string> 'string)
(deftype <structure> 'structure)
(deftype <symbol> 'symbol)
(deftype <u8vector> 'u8vector)
(deftype <u16vector> 'u16vector)
(deftype <u32vector> 'u32vector)
(deftype <u64vector> 'u64vector)
(deftype <unbound> 'unbound)
(deftype <vector> 'vector)
(deftype <void> 'void)
(deftype <f32vector> 'f32vector)
(deftype <f64vector> 'f64vector)

;;; ---------------------------------------------------------------------
;;; the type graph
;;; ---------------------------------------------------------------------
;;; Categories imposes a total order on Scheme types, making them
;;; elements in a graph of classes and schemas. As in Bard, classes
;;; are purely abstract type variables, each of which may refer to any
;;; number of concrete types. schemas are concrete types--types that
;;; may have direct instances. The notational convention is that
;;; classes are named with capitalized nouns, like Class, and schemas
;;; are named with lower-case nouns in angle brackets, like <schema>.

(define $direct-supertypes (make-table test: eqv?))
(define $all-supertypes (make-table test: eqv?))

(define (direct-supertypes tp)
  (table-ref $direct-supertypes tp '()))

(define (all-supertypes tp)
  (let ((all-supers-fn (lambda (t)
                         (c3:compute-precedence t 
                                                direct-supertypes
                                                all-supertypes))))
    (c3:compute-precedence tp direct-supertypes all-supers-fn)))

(define (assert-type! leaf-type direct-supers)
  (let ((cpl (c3:compute-precedence leaf-type direct-supertypes all-supertypes)))
    (table-set! $direct-supertypes leaf-type (map (lambda (x) x) direct-supers))
    (table-set! $all-supertypes leaf-type cpl)))

(assert-type! 'Anything '())

(assert-type! 'Unique '(Anything))
(assert-type! 'Nothing '(Unique))
(assert-type! <null> '(Nothing))
(assert-type! 'Undefined '(Unique))
(assert-type! <unbound> '(Undefined))
(assert-type! 'Void '(Unique))
(assert-type! <void> '(Void))
(assert-type! 'EndOfFile '(Unique))
(assert-type! <eof> '(EndOfFile))

(assert-type! 'Number '(Anything))
(assert-type! 'Real '(Number))
(assert-type! 'Fraction '(Real))
(assert-type! 'Ratio '(Fraction))
(assert-type! <ratnum> '(Ratio))
(assert-type! 'Float '(Fraction))
(assert-type! <flonum> '(Float))
(assert-type! 'Integer '(Real))
(assert-type! 'FixedSizeInteger '(Integer))
(assert-type! <fixnum> '(FixedSizeInteger))
(assert-type! 'FlexibleSizeInteger '(Integer))
(assert-type! <bignum> '(FlexibleSizeInteger))
(assert-type! 'Complex '(Number))
(assert-type! <cpxnum> '(Complex))

(assert-type! 'Boolean '(Anything))
(assert-type! <boolean> '(Boolean))

(assert-type! 'Character '(Anything))
(assert-type! <char> '(Character))

(assert-type! 'Name '(Anything))
(assert-type! 'Keyword '(Name))
(assert-type! <keyword> '(Keyword))
(assert-type! 'Symbol '(Name))
(assert-type! <symbol> '(Symbol))

(assert-type! 'Type '(Anything))
(assert-type! 'Class '(Type Name))
(assert-type! <class> '(Class))
(assert-type! 'Schema '(Type))
(assert-type! <schema> '(Schema))
(assert-type! 'Singleton '(Type))
(assert-type! <singleton> '(Singleton))
(assert-type! 'Structure '(Type))
(assert-type! <structure> '(Structure))
(assert-type! 'JazzObject '(Structure))
(assert-type! <jazz-object> '(JazzObject))
(assert-type! 'MeroonObject '(Structure))
(assert-type! <meroon-object> '(MeroonObject))

(assert-type! 'Foreign '(Anything))
(assert-type! <foreign> '(Foreign))

(assert-type! 'Frame '(Anything))
(assert-type! <frame> '(Frame))

(assert-type! 'Promise '(Anything))
(assert-type! <promise> '(Promise))

(assert-type! 'Procedure '(Anything))
(assert-type! 'Continuation '(Procedure))
(assert-type! <continuation> '(Continuation))
(assert-type! 'Return '(Procedure))
(assert-type! <return> '(Return))
(assert-type! 'Function '(Procedure))
(assert-type! <function> '(Function))
(assert-type! 'Method '(Procedure))
(assert-type! <procedure> '(Method))

(assert-type! 'Container '(Anything))
(assert-type! 'Protocol '(Container))
(assert-type! <protocol> '(Protocol))
(assert-type! 'Sequence '(Container))
(assert-type! 'Vector '(Sequence))
(assert-type! 'String '(Vector))
(assert-type! <string> '(String))
(assert-type! 'ObjectVector '(Vector))
(assert-type! <vector> '(ObjectVector))
(assert-type! 'HomogeneousVector '(Vector))
(assert-type! <s8vector> '(HomogeneousVector))
(assert-type! <u8vector> '(HomogeneousVector))
(assert-type! <s16vector> '(HomogeneousVector))
(assert-type! <u16vector> '(HomogeneousVector))
(assert-type! <s32vector> '(HomogeneousVector))
(assert-type! <u32vector> '(HomogeneousVector))
(assert-type! <f32vector> '(HomogeneousVector))
(assert-type! <s64vector> '(HomogeneousVector))
(assert-type! <u64vector> '(HomogeneousVector))
(assert-type! <f64vector> '(HomogeneousVector))
(assert-type! 'Pair '(Sequence))
(assert-type! <pair> '(Pair))
(assert-type! 'Box '(Container))
(assert-type! <box> '(Box))



