;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          flat-domain-examples.scm
;;;; Project:       Categories
;;;; Purpose:       Example uses of the -flat- domain
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(include "structure-macros.scm")
(include "flat-domain-macros.scm")

;;; using a simple set of structure types

(define print-user (function -flat-))

(define <user> (structure () username password))
(define <admin> (structure (<user>) roles))
(define <trial> (structure () start-date))
(define <trial-user> (structure (<user> <trial>)))

(add-method! print-user
             (flat:method ((u <user>))
                          (newline)
                          (display "User: ")
                          (display (get-key u 'username))
                          (newline)))

(add-method! print-user
             (flat:method ((u <admin>))
                          (newline)
                          (display "Administrator: ")
                          (display (get-key u 'username))
                          (newline)
                          (display "  Roles: ")
                          (display (get-key u 'roles))
                          (newline)))

(add-method! print-user
             (flat:method ((u <trial-user>))
                          (newline)
                          (display "Trial user: ")
                          (display (get-key u 'username))
                          (newline)
                          (display "  Start date: ")
                          (display (get-key u 'start-date))
                          (newline)))

(define $user (make <user> 'username "fred" 'password "dino"))
(define $admin (make <admin> 'username "joe" 'password "f23Zb!" 'roles '(gm dev)))
(define $trial (make <trial-user> 'username "noob" 'password "noob" 'start-date "2009-11-11"))

(print-user $user)
(print-user $admin)
(print-user $trial)

(instance-of? $user <user>) ; => #t
(instance-of? $admin <user>) ; => #f

;;; polymorphic methods on primitive types

(define times (function -flat-))

(add-method! times
             (flat:method ((n <fixnum>)(x <fixnum>))
                          (* n x)))

(add-method! times
             (flat:method ((x <fixnum>)(y <fixnum>)(z <fixnum>))
                          (* x y z)))

(add-method! times
             (flat:method ((n <fixnum>)(s <string>))
                          (apply string-append (vector->list (make-vector n s)))))

(add-method! times
             (flat:method ((n <fixnum>)(s <symbol>))
                          (make-vector n s)))

(times 2 3) ; => 6
(times 2 3 4) ; => 24
(times 2 "Foo") ; => "FooFoo"
(times 2 'bob) ; => #(bob bob)

