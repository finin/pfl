;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: USER -*-
;;; copyright (c) 1985 Tim Finin (tim@cis.upenn.edu)

;;; this file provides the basic PFL functions.

;;; SETTING FUNCTIONS ...

(defun fput (frame slot facet datum
             &key (demons *fdemons*) (type *ftype*)
                  (inherit *finherit*) (number *fnumber*))
  ;; adds a datum to a slot if its not their already.
  (cond ((member datum (fget-local frame slot facet)) datum)
        ((equal facet :value)
         (fput-value frame slot datum demons type inherit number))
        (t (fput-add frame slot facet datum) datum)))

(defun fput-value (frame slot datum demons? type? inherit? number?)
   ;; adds a value to a slot if the types are ok and the
   ;; slot isn't full, then runs demons.
   (unless (and type? (not (fcheck-types frame slot datum)))
      (unless (and number? (not (fcheck-max frame slot)))
         (fput-add frame slot :value datum)
         (if demons?
             (foreach demon in
                   (fget frame slot :if-added :inherit inherit?)
                   (funcall demon frame slot datum)))
          datum)))

(defun fcheck-types (frame slot value)
   ;; true iff value is subsumed by all of the slot's types.
   (forall type in (fget frame slot :type)
      (or (fsubsumes type value)
          (fwarn "~%;; ~S can't fit into ~S of ~S because it's
                  not subsumed by ~S" value slot frame type))))

(defun fcheck-max (frame slot)
   ;; true if there's room for another value.
   (or (<= (length (fget-local frame slot :value))
           (fget-slot-max frame slot))
       (fwarn ";; Can't add another value to ~S of ~S" slot frame)))

(defun fcheck-min (frame slot)
   ;; true if it's ok to remove a value
   (or (> (length (fget-local frame slot :value))
          (fget-slot-min frame slot))
       (fwarn ";; Can't remove a value from ~S of ~S " slot frame)))

(defun fget-slot-max (f s)
  ; returns the max-cardinality for slot S of frame F.
  (let ((max (fget f s :max)))
     (if max (car max) most-positive-fixnum)))

(defun fget-slot-min (f s)
  ; returns the min-cardinality for slot S of frame F.
  (let ((min (fget f s :min)))
     (if min (car min) 0)))

(defun fput-add (frame slot facet datum)
   ;; adds datum to specified (frame,slot,facet)
   (rplacd (last (ffacet frame slot facet)) (list datum)))

(defun ffacet (frame slot facet)
   ;; returns the expression representing the given facet of
   ;; a particular frame and slot, creating it if neccessary.
   (extend facet (extend slot (frame frame))))

(defun extend (key alist)
   ;; like assoc, but adds key KEY if its not in the alist alIST.
   (or (assoc key (cdr alist)) (cadr (rplacd (last alist)(list (list key))))))

;;; ACCESSING FUNCTIONS ...

;; returns the structure which represents the frame named F.
(defun frame (f) (or (get f 'frame) (fcreate f)))

;; returns a list of all local and inherited slots.
(defun fslots (f &key (inherit *finherit*))
   (if inherit
       (collect 'fslots-local (flineage f))
       (fslots-local f)))

(defun fslots-local (f)
  "returns just the local slots of frame f"
  (mapcar #'car (cdr (frame f))))

(defun ffacets (f s &key (inherit *finherit*))
  "returns a list of local and inherited facets for slot of frame"
  (if inherit
      (collect  #'(lambda (x) (ffacets-local x s)) (flineage f))
      (ffacets-local f s)))

(defun ffacets-local (f s) (mapcar 'car (cdr (assoc s (cdr (frame f))))))

(defun fget (frame slot facet &key (inherit *finherit*)
                   (demons *fdemons*) (default *fdefault*))
  (if (equal facet :value)
      (fvalues frame slot :inherit inherit :demons demons :default default)
      (fget1 frame slot facet inherit)))

(defun fget1 (frame slot facet inherit?)
   ;; returns list of data for the given frame, slot and facet
   (or (fget-local frame slot facet)
       (if inherit?
           (forsome parent in (fparents frame)
               (fget1 parent slot facet t)))))

(defun fget-local (frame slot facet)
   ;; returns the data in a facet w/o inheritance or demons.
   (cdr (assoc facet (cdr (assoc slot (cdr (frame frame)))))))

(defun fvalues (f s &key (inherit *finherit*) (demons *fdemons*)
               (default *fdefault*) (finitial f))
   ;; returns values from frame F slot S, local or inherited.
   (or (fget-local f s :value)
       (and default (fget-local f s :default))
       (and demons (forsome demon in (fget-local f s :if-needed)
                      (listify (funcall demon finitial s))))
       (and inherit
            (forsome parent in (fparents f)
                (fvalues parent s :inherit t
                                  :demons demons
                                  :default default
                                  :finitial finitial)))))

(defun listify (l) (if (and l (atom l)) (list l) l))

(defun fvalue (frame slot &key (inherit *finherit*) (demons *fdemons*)
                   (default *fdefault*))
  "returns the 1st value in the specified slot"
   (car (fvalues frame slot :inherit inherit :demons demons :default default)))

;; returns the immediate parents of frame f.
(defun fparents (f) (fget-local f 'ako :value))

;; returns a list of F and all of F's ancestor frames.
(defun flineage (f) (cons f (collect #'flineage (fparents f))))

;;; FUNCTIONS TO REMOVE FRAMES, ETC. ...

(defun fremove (frame slot facet datum
                &key (demons *fdemons*)
                     (inherit *finherit*)
                     (number *fnumber*))
   ;; removes datum from frame's slot's facet and runs if-removed demons.
   (when (and (member datum (fget-local frame slot facet))
              (or (not (eq facet :value)) (fcheck-min frame slot)))
         (delete datum (ffacet frame slot facet))
         (if (and (eq facet :value) demons)
             (foreach demon in (fget frame slot :if-removed :inherit inherit)
                (funcall demon frame slot datum)))))

(defun ferase (f &key (demons *fdemons*) (inherit *finherit*))
  "erases a frame, piece by piece (so that demons can fire)"
  (foreach slot in (append (delete 'ako (fslots-local f)) '(ako))
     (foreach facet in (ffacets-local f slot)
        (foreach datum in (fget-local f slot facet)
           (fremove f s facet datum :demons demons :inherit inherit))))
  (setq *frames* (delete f *frames*))
  (setf (get f 'frame) nil))

;;; PREDICATES

(defun framep (f)
  "returns T iff its argument is a frame"
  (and (symbolp f) (get f 'frame) (member f *frames*)))

(defun fsubsumes (super sub)
  "Does SUPER subsume SUB?  One of {sub,super} must be a frame."
  (or (ako-chain sub super)
      (ako-subsumes-if sub super)
      (ako-subsumed-if sub super)))

(defun ako-chain (sub super)
  "is there a chain of AKO likes from frame SUB to frame SUPER?"
  (and (framep sub) (framep super) (ako-chain1 sub super)))

(defun ako-chain1 (sub super)
  (or (equal sub super)
      (forsome parent in (fparents sub) (ako-chain parent super))))

(defun ako-subsumes-if (sub super)
  "is there a method on SUPER that says SUB is below it?"
  (and (framep super)
       (forsome pred in (fvalues super 'subsumes-if) (funcall pred sub))))

(defun ako-subsumed-if (sub super)
  "is there a method on SUB that says SUPER is above it?"
  (and (framep sub)
       (forsome pred in (fvalues sub 'subsumed-if) (funcall pred sub))))

;;; FUNCTIONS TO CREATE and DEFINE FRAMES

(defmacro fdefineq (frame parents &rest slots)
   ;; defines a frame named FRAME with parents PARENTS and slots SLOTS
   `(fdefine ',frame ',parents ',slots))

(defun fdefine (name parents slots)
   ;; (re)defines a frame, arguments are evaluated.
   (fcreate name)
   (foreach p in (if (listp parents) parents (list parents))
       (fput name 'ako :value p))
   (foreach slot in slots
     (foreach facet in (cdr slot)
        (foreach datum in (cdr facet)
          (fput name (car slot) (car facet) datum))))
   name)

(defun fcreate (f)
  "creates a frame with name F"
  (setq *frames* (adjoin f *frames*))
  (setf (get f 'frame) (list f)))

(provide 'pflbase)
