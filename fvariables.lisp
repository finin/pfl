;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; copyright (c) 1985 Tim Finin (tim@cis.upenn.edu)
;;; This file binds and initializes the global variables used in PFL.

(defvar *frames* nil)    ; a list of all frames in existence.
(defvar *fdemons* t)     ; should demons be triggered by default?
(defvar *finherit* t)    ; should inheritance be done by default?
(defvar *fdefault* t)    ; should default-values be used by default?
(defvar *ftype* t)       ; should type checking be done by default?
(defvar *fnumber* t)     ; should :min and :max checking be done by default?

(provide 'pflvariables)
