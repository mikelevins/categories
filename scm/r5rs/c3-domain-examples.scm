;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          c3-domain-examples.scm
;;;; Project:       Categories
;;;; Purpose:       Example uses of the -c3- domain
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

;;; using a simple set of structure types

(define print-user (function -c3-))

(define <user> (structure () username password))
(c3:derive-type! -c3- <user> (list <anything>))

(define <admin> (structure (<user>) roles))
(c3:derive-type! -c3- <admin> (list <user>))

(define <trial> (structure () start-date))
(c3:derive-type! -c3- <trial> (list <anything>))

(define <trial-user> (structure (<user> <trial>)))
(c3:derive-type! -c3- <trial-user> (list <user> <trial>))

(c3:subtype-of? -c3- <trial-user> <user>) ; => #t
(c3:subtype-of? -c3- <trial-user> <admin>) ; => #f

(add-method! print-user
             (c3:method ((u <user>))
                        (newline)
                        (display "User: ")
                        (display (get-key u 'username))
                        (newline)))

(add-method! print-user
             (c3:method ((u <admin>))
                          (next-method u)
                          (display "Admin roles: ")
                          (display (get-key u 'roles))
                          (newline)))

(add-method! print-user
             (c3:method ((u <trial-user>))
                        (next-method u)
                        (display "[Trial]  Start date: ")
                        (display (get-key u 'start-date))
                        (newline)))


(define $user (make <user> 'username "fred" 'password "dino"))
(define $admin (make <admin> 'username "barney" 'password "bambam" 'roles '(gm editor)))
(define $trial (make <trial-user> 'username "noob" 'password "noob" 'start-date "2009-11-11"))

(print-user $user)
(print-user $admin)
(print-user $trial)

;;; polymorphic methods on primitive types

(define times (function -c3-))

(add-method! times
             (c3:method ((n <number>)(x <number>))
                          (* n x)))

(add-method! times
             (c3:method ((x <number>)(y <number>)(z <number>))
                          (* x y z)))

(add-method! times
             (c3:method ((n <number>)(s <string>))
                          (apply string-append (vector->list (make-vector n s)))))

(add-method! times
             (c3:method ((n <number>)(s <symbol>))
                          (make-vector n s)))

(times 2 3) ; => 6
(times 2 3.0 4.0) ; => 24.0
(times 2 "Foo") ; => "FooFoo"
(times 2 'bob) ; => #(bob bob)
