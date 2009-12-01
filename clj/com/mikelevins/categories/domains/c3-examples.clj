;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          c3-exmaples.clj
;;;; Project:       Categories
;;;; Purpose:       examples using the -c3-domain
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(in-ns 'com.mikelevins.categories.domains.c3)

;;; ======================================================================
;;; -flat- examples
;;; ======================================================================

(def print-user (function -c3-))

(def <user> (structure [] (:username :default false) (:password :default false)))
(derive-type! -c3- <user> (list <anything>))

(def <admin> (structure [<user>] (:roles :default false)))
(derive-type! -c3- <admin> (list <user>))

(def <trial> (structure [] (:start-date :default false)))
(derive-type! -c3- <trial> (list <anything>))

(def <trial-user> (structure [<user> <trial>]))
(derive-type! -c3- <trial-user> (list <user> <trial>))

(add-method! print-user
             (method [[u <user>]]
               (println (format "\nUser: %s" (get-key u :username)))))

(add-method! print-user
             (method [[u <admin>]]
               (next-method u)
               (println (format "\nAdministrator Roles: %s" 
                                (get-key u :roles)))))

(add-method! print-user
             (method [[u <trial-user>]]
               (next-method u)
               (println (format "\n[Trial] Start date: %s" 
                                (get-key u :start-date)))))

(def $user (make <user> :username "fred" :password "dino"))
(def $admin (make <admin> :username "joe" :password "f23Zb!" :roles '(gm dev)))
(def $trial (make <trial-user> :username "noob" :password "noob" :start-date "2009-11-11"))

(print-user $user)
(print-user $admin)
(print-user $trial)


(def times (function -c3-))

(add-method! times
             (method [[n java.lang.Number][x java.lang.Number]]
               (* n x)))

(add-method! times
             (method [[x java.lang.Number][y java.lang.Number][z java.lang.Number]]
               (* x y z)))

(add-method! times
             (method [[n java.lang.Number][s java.lang.String]]
               (apply str (take n (cycle [s])))))

(add-method! times
             (method [[n java.lang.Number][s clojure.lang.Symbol]]
               (take n (cycle [s]))))

(times 2 3) ; => 6
(times 2 3.0 4) ; => 24
(times 2 "Foo") ; => "FooFoo"
(times 2 'bob) ; => #(bob bob)
