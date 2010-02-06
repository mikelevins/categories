;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          flat-exmaples.clj
;;;; Project:       Categories
;;;; Purpose:       examples using the -flat-domain
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(in-ns 'xg.categories.domains.flat)

;;; ======================================================================
;;; -flat- examples
;;; ======================================================================

(def print-user (function -flat-))

(def <user> (structure [] (:username :default false) (:password :default false)))
(def <admin> (structure [<user>] (:roles :default false)))
(def <trial> (structure [] (:start-date :default false)))
(def <trial-user> (structure [<user> <trial>]))

(add-method! print-user
             (method [[u <user>]]
               (println (format "\nUser: %s" (get-key u :username)))))

(add-method! print-user
             (method [[u <admin>]]
               (println (format "\nAdministrator: %s; Roles: %s" 
                                (get-key u :username)
                                (get-key u :roles)))))

(add-method! print-user
             (method [[u <trial-user>]]
               (println (format "\nTrial user: %s; Start date: %s" 
                                (get-key u :username)
                                (get-key u :start-date)))))

(def $user (make <user> :username "fred" :password "dino"))
(def $admin (make <admin> :username "joe" :password "f23Zb!" :roles '(gm dev)))
(def $trial (make <trial-user> :username "noob" :password "noob" :start-date "2009-11-11"))

(print-user $user)
(print-user $admin)
(print-user $trial)


(def times (function -flat-))

(add-method! times
             (method [[n java.lang.Integer][x java.lang.Integer]]
               (* n x)))

(add-method! times
             (method [[x java.lang.Integer][y java.lang.Integer][z java.lang.Integer]]
               (* x y z)))

(add-method! times
             (method [[n java.lang.Integer][s java.lang.String]]
               (apply str (take n (cycle [s])))))

(add-method! times
             (method [[n java.lang.Integer][s clojure.lang.Symbol]]
               (take n (cycle [s]))))

(times 2 3) ; => 6
(times 2 3 4) ; => 24
(times 2 "Foo") ; => "FooFoo"
(times 2 'bob) ; => #(bob bob)
