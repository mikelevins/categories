;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          c3-domain.scm
;;;; Project:       Categories
;;;; Purpose:       The -c3- domain
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(include "structure-macros.scm")

;;; ----------------------------------------------------------------------
;;; ABOUT
;;; ----------------------------------------------------------------------
;;; The -c3- domain implements subtype/supertype relations with
;;; multiple inheritance, using the C3 linearization algorithm
;;; described here:
;;;
;;; http://192.220.96.201/dylan/linearization-oopsla96.html
;;;
;;; The C3 algorithm is implemented in c3.scm, and may be reused for
;;; other domains. 


;;; First version: a C3 domain with no caching (this will be
;;; inefficient, because many unnecessary precedence-list computations
;;; will take place)

(define <anything> (types:make-primitive-type '<anything> (lambda (x) #t)))
(define <list-like> (types:make-primitive-type '<list-like> (lambda (x) (or (list? x)(pair? x)))))

;;; domain data: maintain the graph of types

(define <c3-domain-data> (structure () (direct-supertypes setter: #t)))

(define (c3:make-domain-data)
  (let ((standard-supers `((,<anything> . ())
                           ;; types
                           (,<type> . (,<anything>))
                           (,<primitive-type> . (,<type>))
                           (,<synonym> . (,<type>))
                           (,<category> . (,<type>))
                           (,<structure> . (,<type>))
                           ;; numbers
                           (,<number> . (,<anything>))
                           (,<complex> . (,<number>))
                           (,<real> . (,<number>))
                           (,<rational> . (,<real>))
                           (,<integer> . (,<rational>))
                           (,<flonum> . (,<rational>))
                           (,<fixnum> . (,<integer>))
                           (,<bignum> . (,<integer>))
                           ;; booleans
                           (,<boolean> . (,<anything>))
                           (,<true> . (,<boolean>))
                           (,<false> . (,<boolean>))
                           ;; pairs and lists
                           (,<list-like> . (,<anything>))
                           (,<empty-list> . (,<list-like>))
                           (,<pair> . (,<list-like>))
                           (,<list> . (,<list-like>))
                           ;; ports
                           (,<port> . (,<anything>))
                           (,<input-port> . (,<port>))
                           (,<output-port> . (,<port>))
                           ;; identifiers
                           (,<identifier> . (,<anything>))
                           (,<symbol> . (,<identifier>))
                           (,<keyword> . (,<identifier>))
                           ;; vectors
                           (,<vector> . (,<anything>))
                           (<s8vector> . (,<vector>))
                           (<u8vector> . (,<vector>))
                           (<s16vector> . (,<vector>))
                           (<u16vector> . (,<vector>))
                           (<s32vector> . (,<vector>))
                           (<u32vector> . (,<vector>))
                           (<f32vector> . (,<vector>))
                           (<s64vector> . (,<vector>))
                           (<u64vector> . (,<vector>))
                           (<f64vector> . (,<vector>))
                           ;; procedures
                           (,<callable> . (,<anything>))
                           (,<procedure> . (,<callable>))
                           (,<continuation> . (,<callable>))
                           ;; other types
                           (,<character> . (,<anything>))
                           (,<string> . (,<anything>))
                           (,<end-of-file> . (,<anything>))
                           (,<record> . (,<anything>))
                           (,<foreign> . (,<anything>))
                           (,<gambit-special-value> . (,<anything>)))))
    (make <c3-domain-data> 'direct-supertypes standard-supers)))

;;; supertype relations

(define (c3:direct-supertypes dom t)
  (or (utils:get (get-key (get-key dom 'domain-data) 'direct-supertypes) t eq? #f)
      (error "Type not found in domain: " t dom)))

(define (c3:all-supertypes dom t)
  (let* ((direct-supers-fn (lambda (t)
                             (c3:direct-supertypes dom t)))
         (all-supers-fn (lambda (t) 
                          (c3:compute-precedence t 
                                                 direct-supers-fn
                                                 (lambda (x) (c3:all-supertypes dom x))))))
    (c3:compute-precedence t direct-supers-fn all-supers-fn)))

;;; adding and removing types

(define (c3:derive-type! dom tp supertypes)
  (let* ((data (get-key dom 'domain-data))
         (entries (get-key data 'direct-supertypes))
         (entry (utils:get-entry entries tp eq? #f)))
    (if entry
        (set-cdr! entry (utils:copy-tree supertypes))
        (set-key! data 'direct-supertypes
                  (cons (cons tp (utils:copy-tree supertypes))
                        entries)))))

(define (c3:remove-type! dom tp)
  (let* ((data (get-key dom 'domain-data))
         (entries (get-key data 'direct-supertypes)))
    (set-key! data 'direct-supertypes
              (utils:filter (lambda (e) (not (eq? tp (car e))))
                                entries))))

;;; ----------------------------------------------------------------------
;;; method selection
;;; ----------------------------------------------------------------------
;;; In C3, a method signature is a sequence of types whose length is
;;; equal to the arity of the function. When the function is applied,
;;; methods are selected by matching each input value against the
;;; corresponding type on the signature. A match is positive if the
;;; input's type is equal to or a subtype of the signature
;;; type. Because matches on subtypes are positive, it's possible for
;;; more than one method to match a given set of inputs, and the
;;; selector must therefore be able to order matching methods
;;; according to specificity. When the function is applied, we wish to
;;; execute the most specific method, and supply less-specific methods
;;; in order of decreasing specificity for the use of next-method
;;; calls. Our standard for specificity is this:
;;; 1. if any argument fails to match a method signature, then there
;;;    are no applicable methods
;;; 2. given two applicable signatures S1 and S2, consisting of types
;;;    T0(s1), T1(s1), ... and  T0(s2), T1(s2), ...
;;;    then if T0(s1) is a subype of T0(s2), then S1 is more specific.
;;;    if T0(s2) is a subype of T0(s1), then S2 is more specific.
;;;    If neither is true, then we can't decide based on the first 
;;;    argument position, and we must next check T1(s1) and T1(s2),
;;;    continuing until a more specific signature is determined.
;;;    If we run out of argument positions without making a determination,
;;;    then dispatch is ambiguous and we must signal an error.

(define (c3:signature-equal? s1 s2)
  (and (= (length s1)(length s2))
       (let ((true? (lambda (x) x)))
         (utils:every? true? (map (lambda (x1 x2) (eq? x1 x2)) s1 s2)))))

(define (c3:subtype-of? dom t1 t2)
  (and (utils:position-if (lambda (t) (eq? t t2))
                              (c3:all-supertypes dom t1))
       #t))

;;; return #t if s1 is more specific, #f if s2 is more specific, and
;;; signal a dispatch error if specificity cannot be decided
(define (c3:compare-signatures dom s1 s2)
  (if (or (null? s1)
          (null? s2))
      (error "Ambiguous dispatch")
      (let ((t1 (car s1))
            (t2 (car s2)))
        (if (c3:subtype-of? dom t1 t2)
            #t
            (if (c3:subtype-of? dom t2 t1)
                #f
                (c3:compare-signatures dom (cdr s1) (cdr s2)))))))

(define (c3:order-methods fun vals)
  (let* ((dom (get-domain fun))
         (entries (fun:get-method-entries (get-method-table fun)))
         (candidates (map car (utils:filter (lambda (entry)(= (length vals)(length (car entry))))
                                                entries)))
         (vtypes (map type vals))
         (true? (lambda (x) x))
         (applicable-candidates (utils:filter (lambda (c)
                                                    (utils:every? true? (map (lambda (vt ct) (c3:subtype-of? dom vt ct))
                                                                       vtypes c)))
                                                  candidates))
         (ordered-sigs (utils:sort applicable-candidates 
                                       (lambda (s1 s2) (c3:compare-signatures dom s1 s2))))
         (ordered-meths (map (lambda (s)(utils:get entries s c3:signature-equal? #f))
                             ordered-sigs)))
    (utils:filter true? ordered-meths)))

(define (c3:select-methods fun vals)
  (let* ((meths (c3:order-methods fun vals)))
    (if (null? meths)
        (values #f #f #f)
        (let* ((effectivem (car meths)) 
               (nextm (if (null? (cdr meths))
                          #f
                          (cadr meths)))
               (morems (if nextm
                           (if (null? (cddr meths))
                               #f
                               (cddr meths))
                           #f)))
          (values effectivem nextm morems)))))

(define -c3- (domain 'domain-data (c3:make-domain-data) 
                     'method-selector c3:select-methods))


