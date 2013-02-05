;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.scm
;;;; Project:       Categories
;;;; Purpose:       general utilities
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define (every? fn ls)
  (let loop ((items ls))
    (if (null? items)
        #t
        (if (fn (car items))
            (loop (cdr items))
            #f))))

(define (filter test ls)
  (if (null? ls)
      '()
      (if (test (car ls))
          (cons (car ls)
                (filter test (cdr ls)))
          (filter test (cdr ls)))))

(define (partial fn . args)
  (lambda more-args
    (apply fn `(,@args ,@more-args))))


(define (remove-if test ls)
  (let loop ((items ls))
    (if (null? items)
        '()
        (if (test (car items))
            (remove-if test (cdr items))
            (cons (car items)
                  (remove-if test (cdr items)))))))

(define (some? fn ls)
  (let loop ((items ls))
    (if (null? items)
        #f
        (if (fn (car items))
            (car items)
            (loop (cdr items))))))
