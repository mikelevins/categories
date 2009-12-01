;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          structures.scm
;;;; Project:       Categories
;;;; Purpose:       representation of structures
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; Type Tags
;;; ======================================================================

(define structs:$structure-basis-tag '<structure-basis>)
(define structs:$structure-instance-tag '<structure-instance>)

;;; ======================================================================
;;; Structures
;;; ======================================================================

;;; ABOUT

;;; Structures are just vectors. Access is simply a vector-ref or
;;; vector-set. A separate data structure called a structure-basis
;;; defines a mapping from keys (symbols) to accessors. A getter
;;; function and, optionally, a setter function, is defined for each
;;; key of a structure at the time the structure is defined. These
;;; accessor functions are stored on the structure-basis. Applying a
;;; getter to an instance of the structure returns the value stored in
;;; the element that corresponds to the appropriate key.  Similarly,
;;; applying a setter to a structure instance and another value stores
;;; the value in the appropriate element of the vector.

;;; A structure instance is a vector that contains a reference to the
;;; basis used to instantiate it. Dynamic access to structure slots
;;; using get-key and set-key! is always safe, because these functions
;;; use the embedded reference to the structure-basis to find the
;;; correct accessor functions. A faster form of access is provided by
;;; the functions getter and setter, which return references to the
;;; accessor functions. You can bind an accessor to a variable and
;;; call it directly, bypassing the dynamic lookup of get-key and
;;; set-key!. You do have to take care when doing this, though,
;;; because you lose the safety of the dynamic lookup. If you redefine
;;; a structure basis, get-key and set-key! remain safe to use,
;;; because they lookup their accessors in theembedded
;;; structure-basis. Statically bound accessors are not so safe; if
;;; you accidentally call a getter obtained from the first definition
;;; of your structure basis, bu the parameter is an instance of your
;;; second definition, then the indexes of the structure elements may
;;; have changed, and you may get an incorrect result. So take are
;;; when using statically-bound accessor functions.


(define structs:$structure-metadata-offset
  (+ 1 ; for the structure instance tag
     1 ; for the basis reference
     ))

;;; primitive accessors

(define (structs:basis-keyspecs b)(vector-ref b 1))
(define (structs:basis-predicate b)(vector-ref b 2))

(define (structs:structure-key->index b k) 
  (utils:position-if
   (lambda (bk) (eqv? (car bk) k))
   (structs:basis-keyspecs b)))

(define (structs:structure-basis s)(vector-ref s 1))

(define (structs:slot-ref inst i)
  (vector-ref inst (+ i structs:$structure-metadata-offset)))

(define (structs:slot-set! inst i v)
  (vector-set! inst (+ i structs:$structure-metadata-offset) v))

;;; building structure bases

;;; keyspec: (key default-value make-setter?)
(define (structs:construct-structure-basis keyspecs)
  (let* ((keyspecs (map (lambda (ks i) 
                          (let* ((key (list-ref ks 0))
                                 (default (list-ref ks 1))
                                 (getter (lambda (inst) (structs:slot-ref inst i)))
                                 (setter-flag (list-ref ks 2))
                                 (setter (if setter-flag
                                             (lambda (inst v) (structs:slot-set! inst i v))
                                             (lambda args (error "Key is read-only:" key)))))
                            (list key default getter setter)))
                        keyspecs
                        (utils:range 0 (length keyspecs))))
         (b (vector structs:$structure-basis-tag
                    keyspecs
                    #f))
         (pred (lambda (x) 
                 (and (structs:structure-instance? x)
                      (eq? b (structs:structure-basis x))))))
    (vector-set! b 2 pred)
    b))

(define (structs:get-getter basis key)
  (let ((entry (utils:get-entry (structs:basis-keyspecs basis) key eq? #f)))
    (if entry
        (list-ref entry 2)
        (error "No such key: " key))))

(define (structs:get-setter basis key)
  (let ((entry (utils:get-entry (structs:basis-keyspecs basis) key eq? #f)))
    (if entry
        (list-ref entry 3)
        (error "No such key: " key))))

;;; building structure instances

(define (structs:construct-structure-instance basis)
  (let* ((keyspecs (structs:basis-keyspecs basis))
         (key-count (length keyspecs))
         (element-count (+ key-count structs:$structure-metadata-offset))
         (inst (make-vector (+ structs:$structure-metadata-offset key-count) #f)))
    (vector-set! inst 0 structs:$structure-instance-tag)
    (vector-set! inst 1 basis)
    (do ((i 0 (+ i 1)))
        ((>= i key-count))
      (let* ((keyspec (list-ref keyspecs i))
             (default (list-ref keyspec 1)))
        (structs:slot-set! inst i default)))
    inst))

(define (structs:structure-basis? s)
  (tags:tagged-vector? s structs:$structure-basis-tag))

(define (structs:structure-instance? s)
  (tags:tagged-vector? s structs:$structure-instance-tag))

(define (structs:init-structure-instance inst inits)
  (let loop ((args inits))
    (if (null? args)
        inst
        (if (null? (cdr args))
            (error "Odd number of initialization arguments: " inits)
            (let* ((k (car args))
                   (v (cadr args))
                   (i (structs:structure-key->index (structs:structure-basis inst) k))
                   (remaining (cddr args)))
              (if i
                  (structs:slot-set! inst i v)
                  (error "No such key: " k))
              (loop remaining))))))

;;; ----------------------------------------------------------------------
;;; structure support
;;; ----------------------------------------------------------------------

(define (structs:merge-basis-includes bases keyspecs)
  (let* ((base-keyspecs (map structs:basis-keyspecs bases))
         (bks (if (null? base-keyspecs)
                  '()
                  (utils:reduce utils:merge-rejecting-duplicates 
                                    (car base-keyspecs)(cdr base-keyspecs)))))
    (utils:merge bks keyspecs)))

;;; ----------------------------------------------------------------------
;;; parsing key specs for the structure syntax form
;;; ----------------------------------------------------------------------

(define-syntax structs:parse-key-qualifiers
  (lambda (form)
    (syntax-case form (default: setter:)
      ((_  default: d) (syntax (list d #f)))
      ((_  setter: s) (syntax (list #f s)))
      ((_  default: d setter: s) (syntax (list d s)))
      ((_  setter: s default: d) (syntax (list d s))))))

(define-syntax structs:parse-keyspec
  (lambda (form)
    (syntax-case form ()
      ((_  (k q0 v0 ...)) (syntax (cons 'k (structs:parse-key-qualifiers q0 v0 ...))))
      ((_  k) (syntax (list 'k #f #f))))))

;;; ======================================================================
;;; Public API
;;; ======================================================================

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
  ((setter (structs:structure-basis s) k) s v))

;;; (make structure-basis 'init-key1 init-value1 ...) => #<a-structure-instance>
(define (make basis . inits)
  (structs:init-structure-instance (structs:construct-structure-instance basis)
                                     inits))


