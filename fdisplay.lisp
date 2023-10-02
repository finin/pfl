;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; copyright (c) 1985 Tim Finin (tim@cis.upenn.edu)

;;; This file defines functions to display frames, including:
;;;     fshow - show all data in all facets of all slots of a frame.
;;;     fshow-values - show just values of all slots of a frame.

(defun fshow (frame &key (inherit *finherit*))
  ;; displays a frame
  (format t "~%frame ~S" frame)
  (foreach slot in (fslots frame :inherit inherit)
     (format t "~%  slot ~S:" slot)
     (foreach facet in (ffacets frame slot :inherit inherit)
        (format t "~%    ~S = " facet)
        (foreach datum in (fget frame slot facet :inherit inherit)
           (format t "~S "  datum))))
  frame)

(defun fshow-values (frame
                     &key (inherit *finherit*)
                          (demons *fdemons*)
                          (default *fdefault*))
  ;; displays values in a frame
  (format t "~%frame ~S" frame)
  (foreach slot in (fslots frame :inherit inherit)
     (let ((values (fvalues frame slot
                           :inherit inherit :demons demons :default default)))
       (WHEN values
             (format t "~%  ~S = " slot)
             (foreach v in
                      (if (atom values) (list values) values)
                      (format t "~S " v)))))
  frame)

(provide 'pfldisplay)
