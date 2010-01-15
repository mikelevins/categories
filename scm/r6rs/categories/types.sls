;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.sls
;;;; Project:       Categories
;;;; Purpose:       Categories types
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2010 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(library (categories types)
         (export make-primitive-type
                 primitive-type?
                 
                 synonym
                 synonym?
                 original-type

                 category
                 category?
                 category-members
                 category-member?
                 
                 )
         (import (rnrs))

         

;;; ======================================================================
;;; Primitive types
;;; ======================================================================
         
         (define-record-type primitive-type
           (fields tag predicate))
         
;;; ======================================================================
;;; Synonym types
;;; ======================================================================

         (define-record-type synonym-type
           (fields original-type))

         (define (synonym tp)
           (make-synonym-type tp))

         (define (synonym? tp)
           (synonym-type? tp))

         (define (original-type s)
           (synonym-type-original-type s))

;;; ======================================================================
;;; Category types
;;; ======================================================================

         (define-record-type category-type
           (fields members))

         (define (category . tps)
           (make-category-type tps))

         (define (category? tp)
           (category-type? tp))

         (define (category-members cat)
           (category-type-members cat))
         
         (define (category-member? tp cat)
           (and (memv tp (category-members cat))
                #t))
         
;;; ======================================================================
;;; End library
;;; ======================================================================
         )