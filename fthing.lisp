;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; copyright (c) 1985 Tim Finin (tim@cis.upenn.edu)

;;; this is the default initialization file for the frame hierarchy.
;;; It sets up the hierarchy:
;;;
;;;     thing
;;;       frame         - subsumes all PFL frames.
;;;       slot          - subsumes all PFL slots.
;;;         ako         - the PFL AKO slot.
;;;         instance    - The PFL instance slot.
;;;       expression
;;;         list
;;;         number

(fdefineq thing nil   ; in the beginning was THING ...
  (ako (:type frame)
       (:if-added add-inverse)
       (:if-removed remove-inverse))
  (instance (:type frame)
            (:if-added add-inverse)
            (:if-removed remove-inverse)))

(defun add-inverse (frame slot value)
       ;; add an inverse relation.
      (fput value (fvalue slot 'inverse) ':value frame))

(defun remove-inverse (frame slot value)
       ;; remove an inverse relation
       (fremove value (fvalue slot 'inverse) ':value frame))

(defun add-symmetric (frame slot value) (fput value slot :value frame))

(defun remove-symmetric (frame slot value) (fremove value slot :value frame))

;; these are PFL related concepts....

(fdefineq frame thing (subsumes-if (:value framep)))

(fdefineq slot thing
  (inverse (if-added (lambda (f s d) (fput d 'inverse f)))
           (if-removed (lambda (f s d) (fremove d 'inverse f)))))

(fdefineq ako slot (inverse (:value instance)))

(fdefineq instance slot (inverse (:value ako)))

(fdefineq ILLEGAL nil
  ;; this is a frame that subsumes nothing.
  (subsumes-if (lambda (x) nil)))

;; These are commonly useful concepts.

(fdefineq expression thing (subsumes-if (:value (lambda(x) t))))

(fdefineq list expression (subsumes-if (:value listp)))

(fdefineq number expression (subsumes-if (:value numberp)))

(provide 'pfl-thing)

