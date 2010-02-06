;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.scm
;;;; Project:       Categories
;;;; Purpose:       general utilities
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(define (utils:every? pred ls)
  (or (null? ls)
      (and (pred (car ls))
           (utils:every? pred (cdr ls)))))

(define (utils:any? pred ls)
  (if (null? ls)
      #f
      (or (and (pred (car ls)) (car ls))
          (utils:any? pred (cdr ls)))))

(define (utils:partial fn . args)
  (lambda more-args
    (apply fn `(,@args ,@more-args))))

(define (utils:range x y)
  (letrec ((aux (lambda (x y acc)
                  (if (>= x y)
                      (reverse acc)
                      (aux (+ x 1) y (cons x acc))))))
    (aux x y '())))

(define (utils:copy-tree x)
  (let recur ((x x))
    (if (not (pair? x)) x
        (cons (recur (car x)) (recur (cdr x))))))

(define (utils:position-if pred ls)
  (letrec ((aux (lambda (pred ls i)
                  (if (null? ls)
                      #f
                      (if (pred (car ls))
                          i
                          (aux pred (cdr ls) (+ i 1)))))))
    (aux pred ls 0)))

(define (utils:remove item ls test)
  (letrec ((rm (lambda (item ls test acc)
                 (if (null? ls)
                     (reverse acc)
                     (if (test item (car ls))
                         (rm item (cdr ls) test acc)
                         (rm item (cdr ls) test (cons (car ls) acc)))))))
    (rm item ls test '())))

(define (utils:get-entry alist key pred default)
  (let loop ((entries alist))
    (if (null? entries)
        default
        (if (pred key (car (car entries)))
            (car entries)
            (loop (cdr entries))))))

(define (utils:get alist key pred default)
  (let loop ((entries alist))
    (if (null? entries)
        default
        (if (pred key (car (car entries)))
            (cdr (car entries))
            (loop (cdr entries))))))

(define (utils:filter pred ls)
  (letrec ((aux (lambda (pred ls acc)
                  (if (null? ls)
                      (reverse acc)
                      (if (pred (car ls))
                          (aux pred (cdr ls) (cons (car ls) acc))
                          (aux pred (cdr ls) acc))))))
    (aux pred ls '())))

(define (utils:merge base-alist new-alist)
  (append (utils:filter (lambda (entry) (not (assq (car entry) new-alist)))
                            base-alist) 
          new-alist))

(define (utils:merge-rejecting-duplicates base-alist new-alist)
  (append (map (lambda (entry) 
                 (if (assq (car entry) new-alist)
                     (error "Duplicate key: " (car entry))
                     entry))
               base-alist) 
          new-alist))

(define (utils:reduce fn base-val more-vals)
  (if (null? more-vals)
      base-val
      (utils:reduce fn (fn base-val (car more-vals)) (cdr more-vals))))

;;; From Sort.scm
;;; Copyright (c) 2006-2008 by Marc Feeley, All Rights Reserved.
;;; slightly modified by mikel evins

(define (utils:sort sequence less?)
  (define (sort-list lst less?)
    (define (mergesort lst)
      (define (merge lst1 lst2)
        (cond ((not (pair? lst1))
               lst2)
              ((not (pair? lst2))
               lst1)
              (else
               (let ((e1 (car lst1)) (e2 (car lst2)))
                 (if (less? e1 e2)
                     (cons e1 (merge (cdr lst1) lst2))
                     (cons e2 (merge lst1 (cdr lst2))))))))
      (define (split lst)
        (if (or (not (pair? lst)) (not (pair? (cdr lst))))
            lst
            (cons (car lst) (split (cddr lst)))))
      (if (or (not (pair? lst)) (not (pair? (cdr lst))))
          lst
          (let* ((lst1 (mergesort (split lst)))
                 (lst2 (mergesort (split (cdr lst)))))
            (merge lst1 lst2))))
    (mergesort lst))
  (cond ((not (procedure? less?))
         (error "procedure expected"))
        ((or (null? sequence)
             (pair? sequence))
         (sort-list sequence less?))
        ((vector? sequence)
         (list->vector (sort-list (vector->list sequence) less?)))
        (else
         (error "vector or list expected"))))