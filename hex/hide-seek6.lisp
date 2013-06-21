;;; File: hide-seek.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The Hide-Seek Hexagonal World Environment

;;; ===========================================================
;;; This definition should be changed to obtain different testing environment
;;; At least, you will put your agent name instead of ask-user-hs-agent6
;;;
(defstructure (hs-world6
		(:include
		  hexagrid-environment
		  (size (@ 10 10))               ; 10 x 10 grid environment
		  (aspec '(ask-user-hs-agent6))  ; one seeking agent, YOU PUT YOUR AGENT HERE
		  (bspec '((at edge wall)        ; grid edges are wall
			   (* 5 bush)            ; 5 bushes at random locations
			   (* 5 (C bush person)) ; 5 more bushes with hidden persons
			   ))))
	      (persons-remaining 5))             ; should correspond to hidden persons
;;; ==========================================================

(defstructure (hs-agent6
                (:include gui-agent (body (make-hs-agent6-body))))
  "An agent for hs world.")

(defstructure (hs-agent6-body (:include agent-body))
  (grid nil))

(defstructure (ask-user-hs-agent6 (:include hs-agent6 (key-input-program 'ask-user-hs)))
  "An agent that asks the user to type in an action.")

(defun ask-user-hs (percept key)
  "Let user decide, what action to take, by pressing a key."
  (cond ((sdl:key= key :sdl-key-q)
	 'northwest)
	((sdl:key= key :sdl-key-w)
	 'north)
	((sdl:key= key :sdl-key-e)
	 'northeast)
	((sdl:key= key :sdl-key-a)
	 'turnleft)
	((sdl:key= key :sdl-key-d)
	 'turnright)
	((sdl:key= key :sdl-key-x)
	 'stop)
	((sdl:key= key :sdl-key-p)
	 'pyky)
	)
  )

;;; ================================================================
;;; This is to be defined when designing a new agent
;
;(defstructure (xxx    ; replace xxx by your unique name, as e.g. FIT username
;                (:include hs-agent6
;                  (body (make-xxx-body))
;                  (program 'xxx_program)))
;  "Your agent for hs-world6.")
;
;(defstructure (xxx-body
;                (:include hs-agent6-body))
;  (slot1 default1)  ; any specific extra slots your agent's body would need
;  ...
;  (slotn defaultn))
;
;(defun xxx_program (percept)
;  (let ((agent-body (first percept)))    ; extracts agent body from percept
;    (setf percept (rest percept))      ; extracts proper percept
; ...
; ...  here your program comes to calculate and return a proper action
; ...
;) )
;;; Any number of auxiliary functions can follow
;;;
;;; To test run the game you perform
;;; (run-gui (make-hs-world6))
;;; ==================================================================

(defstructure (bush (:include obstacle
    (alive? t)
    (name "B")
    (size 0.9)
    ))
  "A bush that can serve to hide persons protecting them from 3 directions.")

;;;; Drawing of structures above

(defmethod draw-object ((obj bush) surface)
  (let* ((size		(sdl:width surface))
	 (padding	(/ (* size 19) 20))
	 (new-size	(- size (* 2 padding)))
	 )
    (draw-hexagon padding padding new-size (convert-color 'brown) :surface surface)
    )
  )

(defmethod info-text-environment ((env hs-world6) lines)
  (let ((next (call-next-method))
	(persons (hs-world6-persons-remaining env))
	)
    (setf (first next)
	  (concatenate 'string 
		       (first next)
		       (format nil
			       (if (> persons 1)
				 "there are ~D hidden persons." 
				 "there is ~D hidden person."
				 )
			       persons)
		       )
	  )
    (when (and (eq (environment-step env) 0)
	       (eq (first (grid-environment-aspec env)) 'ask-user-hs-agent6)
	       )
      (setf (second next)
	    "Press ESC to exit, QWE to move, AD to turn and P to pyky.")
      )
    next
    )
  )

;;;; Defining the generic functions

(defmethod termination? ((env hs-world6))
  "End when everyone dead."
  (or (> (environment-step env) (environment-max-steps env))
      (zerop (hs-world6-persons-remaining env))))

(defmethod initialize ((env hs-world6))
  "Tests if persons could be visible at all."
  (call-next-method)
  (check-persons-visible (grid-environment-grid env) env))

(defun check-persons-visible (grid env)
  (let* ((dim (array-dimensions grid))
         (xmax (- (xy-x dim) 2))
         (ymax (- (xy-y dim) 2)))
    (dotimes (x xmax)
      (dotimes (y ymax)
        (let* ((loc (@ (1+ x) (1+ y)))
               (bush (find-object-if #'bush-p loc env))
               (person (and bush (find-if #'person-p (object-contents bush))))
               (heading (and person (object-heading bush))))
          (when (and person (not (empty-loc? (xy-update loc heading) env)))
            (check-bush bush loc grid env)))))))

(defun check-bush (bush loc grid env)
  "Checks and makes the person in bush visible from somewhere."
  (let ((free-heading (get-free-hd loc env)))
    (if free-heading
        (setf (object-heading bush) free-heading)
        (delete-obstacle bush loc grid env))))

(defun get-free-hd (loc env)
  "Finds a free neighbor of loc or occupied by an agent."
  (let ((hd-list '((0 1) (1 1) (1 0) (0 -1) (-1 0) (-1 1))))
    (dolist (hd hd-list)
      (let ((neighbor (xy-update loc hd)))
        (when (or (null (grid-contents env neighbor))
                  (find-object-if #'agent-p neighbor env))
          (return-from get-free-hd hd))))
    (dolist (hd hd-list)
      (let ((neighbor (xy-update loc hd)))
        (when (find-object-if #'bush-p neighbor env)
          (let ((grid (grid-environment-grid env))
                (nx (xy-x neighbor))
                (ny (xy-y neighbor)))
            (setf (aref grid nx ny) nil)
            (return-from get-free-hd hd)))))))

(defmethod performance-measure ((env hs-world6) agent)
  "Score 20 for finding a person and penalty of 1 for each step taken."
  (let ((agent-body (agent-body agent)))
    (- (* (length (object-contents agent-body)) 20)
       (environment-step env))))

(defmethod get-percept ((env hs-world6) agent)
  "Perceive nil until the first object seen in every direction from where agent is heading."

  (let ((loc (object-loc (agent-body agent)))
	(heading (object-heading (agent-body agent)))
	)
    ; start bfs from one position forward
    (run-percept-bfs (update-loc loc heading) heading env (agent-body agent))
    )
  (setf *percept* (cons (agent-body agent) *percept*))

  ;(format t "~&Percept is ~A" *percept*)
  *percept*
  )

(defmethod create-percept-loc ((env hs-world6) loc heading agent-body reveal-program)
  "Creates percept for one location."
  (let* ((bush    (find-object-if #'bush-p loc env))
	 (person  (and bush (find-if #'person-p (object-contents bush))))
	 (wall    (find-object-if #'wall-p loc env))
	 (obj     (find-object-if #'object-p loc env))
	 (bump    (object-bump agent-body))
	 result)
    ; create percept
    (setf result
	  (list (cond
		  ((and bush person
			(xy-equal heading (looks-where loc env)))
		   'person)
		  (bush 'bush)
		  (wall 'wall)
		  (obj 'obstacle)))
	  )
    (let* (
	   (visible (node-visibility loc))
	   (color (convert-color
		    (if visible (object-color agent-body) 'grey)))
	   )
      ; draw percept box on the screen
      (draw-percept loc env color :hexagon T)
      ; revealed person?
      (if (and visible (eq (first result) 'person))
	(if (eq reveal-program #'nothing)
	  ; draw revealed person
	  (draw-percept loc env sdl:*white* :circle T)
	  ; else call function on the revealed person
	  (funcall reveal-program env agent-body bush person)
	  )
	)
      )
    result
    )
  )

(defun looks-where (loc env)
  (let ((object (find-object-if #'bush-p loc env)))
    (if object (tsouth (copy-list (object-heading object))))
    )
  )

(defmethod legal-actions ((env hs-world6))
  "In the hide-seek world, agent can move around and pyky."
  '(north northwest northeast pyky turnleft turnright stop))

;;;; Actions

(defmethod pyky ((env hs-world6) agent-body &optional args)
  "Agent heading to a neighbor bush with person picks the person."
  (let ((loc (object-loc agent-body))
	(heading (object-heading agent-body))
	(remaining (hs-world6-persons-remaining env))
	)
    ; start bfs from one position forward with the special argument
    (run-percept-bfs (update-loc loc heading) heading env agent-body :reveal-program #'pyky-location)
    (when (>= (hs-world6-persons-remaining env) remaining)
      ; did not found
      (setf (environment-illegal-actions env)
	    (nconc (environment-illegal-actions env)
		   (list agent-body 'pyky)))
      )
    )
  )

(defun pyky-location (env agent-body bush person)
  "Agent pykys a bush with a hidden person."
  (setf (object-contents bush)
	(delete person (object-contents bush)))
  (place-in-container person agent-body env)
  (decf (hs-world6-persons-remaining env))
  )

(defmethod turnleft ((env hs-world6) agent-body &optional args)
  "Turn left."
  (setf (object-heading agent-body) (tnorthwest (object-heading agent-body))))

(defmethod turnright ((env hs-world6) agent-body &optional args)
  "Turn left."
  (setf (object-heading agent-body) (tnortheast (object-heading agent-body))))

(defmethod stop ((env hs-world6) agent-body &optional args)
  "Stop hs world execution."
  (setf (environment-max-steps env) 0))

(defmethod northwest ((env hs-world6) agent-body &optional (args 1))
  "Go args steps to the northwest from where heading."
   (let ((heading (tnorthwest (copy-list (object-heading agent-body)))))
     (dotimes (i args)
       (move-object-by agent-body heading env))))

(defmethod northeast ((env hs-world6) agent-body &optional (args 1))
  "Go one step to the northeast from where heading."
   (let ((heading (tnortheast (copy-list (object-heading agent-body)))))
     (dotimes (i args)
       (move-object-by agent-body heading env))))

(defmethod north ((env hs-world6) agent-body &optional (args 1))
  "Go args steps to where heading."
   (let ((heading (object-heading agent-body)))
     (dotimes (i args)
       (move-object-by agent-body heading env))))

; (run-gui (make-hs-world6))

