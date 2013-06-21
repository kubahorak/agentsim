;;; File: hexagrid-env.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; Environments with a two-dimensional hexagrid layout occupied by objects

;;; This file defines a HEXAGRID-ENVIRONMENT, a kind of environment where there is
;;; a hexagonal grid of spaces, each potentially containing objects.

(defstructure (hexagrid-environment (:include grid-environment))
	      (info-lines 2) ; Number of lines of informational texts
  )

(defstructure object
  "An object is anything that occupies space.  Some objects are 'alive'."
  (name "?")			; Used to print the object on the map
  (alive? nil)                  ; Is the object alive?
  (loc (@ 1 1))			; The square that the object is in
  (bump nil)			; Has the object bumped into something?
  (size 0.5)			; Size of object as proportion of loc
  (color 'white)		; Some objects have a color
  (shape 'rectangle)		; Some objects have a shape
  (sound nil)			; Some objects create a sound
  (contents '())		; Some objects contain others
  (max-contents 0.4)            ; How much (total size) can fit inside?
  (container nil)		; Some objects are contained by another
  ; clockwise			NORTH N-E    S-E  SOUTH  S-W    N-W
  (heading (my-nth (random 6) '((0 1) (1 1) (1 0) (0 -1) (-1 0) (-1 1)))) ; Direction
)

(defun my-nth (n seq) (copy-list (nth n seq)))

(defstructure (obstacle (:include object (name "#") (color 'brown))))

(defstructure (wall (:include obstacle (color 'black))))

(defstructure (agent-body (:include object (alive? t) (name nil) (color 'red)))
  "An agent body is an object; some bodies have a hand that can hold 1 thing."
  (holding nil))

(defstructure (person (:include object
    (name "P")
    (size 0.01)))
  "An object to be find by an agent.")

;;;; Generic object functions

(defmethod draw-object ((obj object) surface)
  "Draws unspecified object on the surface."
  (let* ((size (sdl:width surface))
	 (half (/ size 2))
	 (r    (/ size 4)))
    (sdl:draw-filled-circle-* (round half) (round half) (round r) :surface surface)
    )
  )

(defmethod draw-object ((obj obstacle) surface)
  "Draws an obstacle on the surface."
  (let* ((size		(sdl:width surface))
	 (padding	(/ (* size 19) 20))
	 (new-size	(- size (* 2 padding)))
	 (color		(convert-color (object-color obj)))
	 )
    (draw-hexagon padding padding new-size color :surface surface)
    )
  )

(defmethod draw-object ((obj wall) surface)
  "Draws wall on the surface."
  (let* ((size		(sdl:width surface))
	 (padding	(/ (* size 19) 20))
	 (new-size	(- size (* 2 padding)))
	 (color		(convert-color (object-color obj)))
	 )
    (draw-hexagon padding padding new-size color :surface surface)
    )
  )

(defmethod draw-object ((obj agent-body) surface)
  "Draws agent-body on the surface."
  (let* ((size	(sdl:width surface))
	 (half	(/ size 2))
	 (r	(/ size 4))
	 (color	(convert-color (object-color obj)))
	 )
    (sdl:draw-filled-circle-* (round half) (round half) (round r) :color color :surface surface)
    ; rotation
    (let* ((p	(* 0.75 r))
	   ; vertices from the center to point A,B,C
	   (va	(@ (- p) (- p)))
	   (vb	(@ p     (- p)))
	   (vc	(@ 0     (- half)))
	   (angle (angle-from-heading (object-heading obj)))
	   )
      (rotate-vertex va angle)
      (rotate-vertex vb angle)
      (rotate-vertex vc angle)
      ; final points of the triangle
      (let ((a	(sdl:point :x (round (+ half (xy-x va))) :y (round (+ half (xy-y va)))))
	    (b	(sdl:point :x (round (+ half (xy-x vb))) :y (round (+ half (xy-y vb)))))
	    (c	(sdl:point :x (round (+ half (xy-x vc))) :y (round (+ half (xy-y vc)))))
	    )
	(if sdl:*gfx-loaded-p*
	  (sdl:draw-filled-polygon (list a b c) :color color :surface surface)
	  (sdl:draw-polygon (list a b c) :color color :surface surface))
	)))
  )

;;;; Generic environment functions

(defmethod update-fn ((env grid-environment))
  "Execute the actions and do bookkeeping on the bump sensor."
  (for each agent in (environment-agents env) do
       (setf (object-bump (agent-body agent)) nil)) ; dissipate bumps
  (execute-agent-actions env))

(defmethod legal-actions ((env grid-environment))
  '(turn forward grab release speak))

(defmethod initialize ((env grid-environment))
  "Build a new environment with all the agents and objects in place.
  This gets passed an environment which may need to have the objects placed.
  See PARSE-SPECS below in this file for more on initialization."
  (unless (environment-initialized env)
    ;; Build the grid and place objects where they belong
    (setf (grid-environment-grid env)
	  (make-array (grid-environment-size env) :initial-element '()))
    (parse-specs env (grid-environment-bspec env))
    (parse-specs env (grid-environment-cspec env))
    (parse-specs env (grid-environment-aspec env))
    (setf (environment-agents env) (reverse (environment-agents env)))
    (call-next-method)
) )

(defmethod termination? ((env grid-environment))
  "By default, we stop when there are no live agents."
  (every #'(lambda (agent) (not (object-alive? (agent-body agent))))
	 (environment-agents env)))

; overrides
(defmethod display-environment-snapshot ((env hexagrid-environment))
  ; IDEA Here one could divide surface, so that two objects could occupy one cell
  (draw-board env :key #'(lambda (surface objects)
			   (mapcar #'(lambda (object)
				       (draw-object object surface)
				       )
				   objects)
			   ))
  ; Set percept for each agent
  (for each agent in (environment-agents env) do
       (setf (agent-percept agent) (get-percept env agent))
       )
  (sdl:update-display)
  )

(defmethod print-structure ((object object) stream)
  "Show an object's name, and if it is alive, the direction it faces."
  (let ((name (or (object-name object) (type-of object)))
        (cont (object-contents object)))
    (if (object-alive? object)
        (if cont
            (format stream "~A.~A" name (heading->string (object-heading object)))
	    (format stream "~A~A" name (heading->string (object-heading object))))
        (if cont
            (format stream "~A." name)
            (format stream "~A" name)))))

(defmethod info-text-environment ((env hexagrid-environment) lines)
  "Returns info about the environment."
  (let* ((str1 (format nil "~&At time step ~D, " (environment-step env)))
	(str (cons str1 '()))
	)
    (if (> (environment-step env) 0)
      (for each agent in (environment-agents env) do
	   (setf str (append str (list
			   (format nil
				   "Agent ~A has score ~D and does ~A. "
				   (object-color (agent-body agent)) (agent-score agent) (agent-action agent)
				   ))))
	   )
      ; in the first step display keyboard help
      (setf str (append str (list "Press ESC to exit, numeric keys forward the time.")))
      )
    str
    )
  )

;;;; Actions

(defmethod speak ((env grid-environment) agent-body sound) ; speak
  "The agent emits a sound."
  (declare-ignore env)
  (setf (object-sound agent-body) sound))

(defmethod turn ((env grid-environment) agent-body direction)
  "The agent changes its heading by turning right or left."
  (declare-ignore env)
  (let* ((headings '#((0 1) (1 1) (1 0) (0 -1) (-1 0) (-1 1)))
	 (now (position (agent-body-heading agent-body) headings
			:test #'equal))
	 (delta (case direction (right -1) (left +1) (t 0))))
    (setf (object-heading agent-body)
	  (elt headings (mod (+ now delta) 4)))))

(defmethod forward ((env grid-environment) agent-body)
  "Move the object to the location that is one step directly ahead of it."
  (move-object-to
   agent-body
   (add-locs (object-loc agent-body) (object-heading agent-body))
   env))

(defmethod grab ((env grid-environment) agent-body &optional args)
  "Grab an object at the specified location.  Assumes a one-handed agent."
  (declare-ignore args) ;; They are used in other environments
  (let ((object (find-object-if #'grabable? (object-loc agent-body) env)))
    (when (and object
	       (not (agent-body-holding agent-body))
	       (place-in-container object agent-body env))
      (setf (agent-body-holding agent-body) object))))

(defun grabable? (object)
  (and (not (obstacle-p object)) (not (agent-body-p object))))

(defmethod release ((env grid-environment) agent-body &optional args)
  "Release an object that is in the hand, putting it at the specified loc."
  (declare-ignore args) ;; They are used in other environments
  (let ((object (agent-body-holding agent-body)))
    (when object
      (place-object object (object-loc agent-body) env)
      (setf (agent-body-holding agent-body) nil))))

;;;; Initializing Environments

;;; The grammar for the object-specs language is as follows:
;;;<PRE>
;;;   specs  =>  (spec...)
;;;   spec   =>  (AT where what...) | (* n spec...) | what
;;;   where  =>  EDGE | ALL | FREE? | START | (x y) | (AND where...)
;;;   what   =>  object | type | (type arg...) | (* n what...)  | (P x what...) |
;;;              (U what ...) | (C what what)
;;;   n      =>  integer | (+- integer integer)
;;;
;;; The location FREE? means a randomly chosen free loc, ALL means every loc.
;;; If no location is specified, the default is START for agents, FREE?
;;; otherwise.
;;;
;;; Examples of spec:
;;;
;;;  (at edge wall)                  1 wall in every perimeter location
;;;  (at free? wumpus)               1 wumpus in some random free location
;;;  wumpus                          Same as above
;;;  ask-user-agent                  1 ask-user-agent in the start cell
;;;  (* 2 apple)                     An apple in each of 2 random locations
;;;  (* 2 (apple :color green))      A green apple in each of 2 random locs
;;;  (at all (p 0.25 dirt))          All free locations have 1/4 chance of dirt
;;;  (at (2 3) (* 8 apple) sign)     Location (2 3) has 8 apples and a sign
;;;  (* (+- 10 4) apple)             10 plus or minus 4 (at random) apples
;;;  (at (and (1 2) (1 4)) cashier)  These two locations each get a cashier
;;;  (* 2 smoke fire)                2 random locs get both smoke and fire
;;;</PRE>

(defun parse-specs (env specs)
  "Place objects, defined by specs, in the environment."
  (for each spec in specs do
       (parse-spec env spec)))

(defun parse-spec (env spec)
  (case (op spec)
   (AT (parse-where env (arg1 spec) (rest (args spec))))
   (*  (for i = 1 to (parse-n (arg1 spec)) do
	 (parse-specs env (rest (args spec)))))
   (t  (parse-what env nil spec))))

(defun parse-where (env where whats)
  (cond
   ((eq where 'EDGE)    (let ((x-size (xy-x (grid-environment-size env)))
			      (y-size (xy-y (grid-environment-size env))))
			  (for i = 0 to (- x-size 1) do
			       (parse-whats env (@ i 0) whats)
			       (parse-whats env (@ i (- y-size 1)) whats))
			  (for i = 1 to (- y-size 2) do
			       (parse-whats env (@ 0 i) whats)
			       (parse-whats env (@ (- x-size 1) i) whats))))
   ((eq where 'ALL)     (dotimes (x (xy-x (grid-environment-size env)))
			  (dotimes (y (xy-y (grid-environment-size env)))
			    (when (free-loc? (@ x y) env)
			      (parse-whats env (@ x y) whats)))))
   ((eq where 'FREE?)   (parse-whats env (random-loc env :if 'free-loc?) whats))
   ((eq where 'START)   (parse-whats env (grid-environment-start env) whats))
   ((xy-p where)        (parse-whats env where whats))
   ((eq (op where) 'AND)(for each w in (args where) do
			     (parse-where env w whats)))
   (t (warn "Unrecognized object spec ignored: ~A" `(at ,where ,@whats)))))

(defun parse-whats (env loc what-list)
  (for each what in what-list do
       (parse-what env loc what)))

(defun parse-what (env loc what)
  "Place the objects specified by WHAT-LIST at the given location
  The default location is START for an agent, random otherwise.
  The notation (P 0.5 what...) means 50% chance of placing each what,
  and (* n what...) means place n copies of each what."
  (case (op what)
    (* (for i = 1 to (parse-n (arg1 what)) do
	 (parse-whats env loc (rest (args what)))))
    (P (for each w in (rest (args what)) do
	    (when (< (random 1.0) (arg1 what))
	      (parse-what env loc w))))
    (U (let ((location (if loc loc (random-loc env :if #'free-loc?))))
         (for each w in (args what) do
	      (parse-what env location w))))
    (C (let* ((container (parse-what env loc (arg1 what)))
              (contents (parse-what env loc (arg2 what))))
         (place-in-container contents container env)))
    (t (let* ((object (if (object-p what) what
			(apply #'make (op what) (args what))))
	      (location (or loc (if (agent-p object)
				    (grid-environment-start env)
				    (random-loc env :if #'free-loc?)))))
	 (place-object object location env t)))))

(defun parse-n (n)
  (if (eq (op n) '+-)
      (round (+ (arg1 n) (random (float (arg2 n)))
		(- (random (float (arg2 n))))))
      n))

(defun make (type &rest args)
  "Make an instance of the specified type by calling make-TYPE."
  (apply (concat-symbol 'make- type) args))

(defun random-loc (env &key (if #'true) (tries 100))
  "Return a random loc, somewhere in the environment.
  The loc must satisfy the :IF predicate.  If it can't find such a location
  after a number of TRIES, it signals an error."
  (or (for i = 1 to tries do
	   (let ((loc (mapcar #'random (grid-environment-size env))))
	     (when (funcall if loc env) (RETURN loc))))
      (error "Can't find a location.")))

(defun free-loc? (loc env)
  "A location is free if there is no obstacle there and it is not the start."
  (and (empty-loc? loc env)  ;(not (find-object-if #'obstacle-p loc env))
       (not (equal loc (grid-environment-start env)))))

(defun empty-loc? (loc env)
  (let ((g-cont (grid-contents env loc)))
    ; (format t "~%location: ~S  contants: ~S~%" loc g-cont)
    (null g-cont)))

(defun rotate-vertex (xy a)
  "Rotates vertex xy by angle a in degrees clockwise."
  ; x' = x cos(a) - y sin(a)
  ; y' = x sin(a) + y cos(a)
  (let ((x (xy-x xy))
	(y (xy-y xy))
	; from degrees to radians
	(a (* PI (/ a 180)))
	)
    (setf (xy-x xy) (- (* x (cos a)) (* y (sin a))))
    (setf (xy-y xy) (+ (* x (sin a)) (* y (cos a))))
    )
  )

(defun angle-from-heading (heading)
  "Returns clockwise angle difference from the top vector."
  (case (xy-y heading)
    (-1 180)
    (0 (case (xy-x heading)
	 (-1 240)
	 (1  120)
	 ))
    (1 (case (xy-x heading)
	  (-1 300)
	  (0  0)
	  (1  60)
	  ))
    )
  )

; overrides grid.lisp
(defun move-object-by (object delta env)
  "Move an object by delta location and return that location.  However,
  attempting to move into a location with an obstacle fails (returns nil)
  and the object receives a bump."
  (let ((hexx (xy-x delta))
	(hexy (xy-y delta))
	(objx (xy-x (object-loc object)))
	(objy (xy-y (object-loc object))))
    (if (not (eq hexx 0)) ; only when moving sideways
      (if (eq (mod objx 2) 0) ; even column
	 (decf hexy 1)
       )
      )
    (move-object-to object
		    (xy-add (@ hexx hexy) (object-loc object))
		    env)))

(defun heading->string (heading)
  "Convert a heading like (0 1) to a depictive string like ^."
  (cond 
    ((equal heading '(0 1)) "^")
    ; north-east
    ((equal heading '(1 1)) "/")
    ; south-east
    ((equal heading '(1 0)) ">")
    ((equal heading '(0 -1)) "V")
    ; south-west
    ((equal heading '(-1 0)) "<")
    ; north-west
    ((equal heading '(-1 1)) "\\")
    (t "?")))

; overrides grid.lisp
; two agents can not share a location
(defun move-object-to (object loc env)
  "Move an object to an absolute location and return that location.  However,
  attempting to move into a location with an obstacle fails (returns nil)
  and the object receives a bump."
  (let ((size (grid-environment-size env)))
    (when (inside loc (xy-x size) (xy-y size))
      (cond ((find-object-if #'obstacle-p loc env)
	     (setf (object-bump object) 'bump)
	     nil)
	    ((find-object-if #'agent-body-p loc env)
	     (setf (object-bump object) 'bump)
	     nil)
	    (t (remove-object object env)
	       (place-object object loc env)
	       loc)))))

