;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; copyright (c) 1985 Tim Finin (tim@cis.upenn.edu)
;;; local macros and utilities used in PFL.


;; syntactic sugar for a mapcar, some and every.
(defmacro foreach (V in L &rest body) `(mapcar #'(lambda (,V) ,@body) ,L))

(defmacro forsome (V in L &rest body) `(some #'(lambda (,V) ,@body) ,L))

(defmacro forall (V in L &rest body) `(every #'(lambda (,V) ,@body) ,L))

(defmacro fwarn (msg &rest fillers) `(progn (format t ,msg ,@fillers) nil))

;; applies a function to a list of things, then unions the results.
(defun collect (function sequence)(reduce #'onion (mapcar function sequence)))

;; like union, but works with 0 and 1 argument.
(defun onion (&optional arg1 arg2) (union arg1 arg2))

(provide 'pflmacros)
