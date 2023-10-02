;;; -*- Mode: LISP; Syntax: Zetalisp; Base: 10 -*-
;;; copyright (c) 1985 Tim Finin (tim@cis.upenn.edu)
;;; this file defines/loads the PFL system.

#+ symbolics
(defpackage pfl
   (:export fput frame fslots ffacets fget fvalues fremove ferase
            framep fsubsumes fdefineq fdefine ako instance subsumes-if
            subsumed-if))

#+ symbolics
(defsystem pfl
   (:name "Pedagogic Frame Representation Language")
   (:package "pfl")
   (:pathname-default "upenn:usr:[tim.frames]")
   (:module pflvariables ("pflvariables"))
   (:module pflmacros ("pflmacros"))
   (:module pflbase ("pflbase"))
   (:module pfldisplay ("pfldisplay"))
   (:module pflthing ("thing"))

   (:compile-load pflvariables)
   (:compile-load pflmacros)
   (:compile-load pflbase (:fasload pflmacros))
   (:compile-load pfldisplay (:fasload pflmacros))
   (:load pflthing))

#+vax
(progn
  ;; VAXLISP system file for PFL.
  (require 'pflvariables "pflvariables.lisp")
  (require 'pflmacros "pflmacros.lisp")
  (require 'pflbase "pflbase.lisp")
  (require 'pfldisplay "pfldisplay.lisp")
  (require 'pflthing "pflthing.lisp")
  (export '(fput frame fslots ffacets fget fvalues fremove ferase
            framep fsubsumes fdefineq fdefine
            ako instance subsumes-if subsumed-if))
  (provide 'pfl))

