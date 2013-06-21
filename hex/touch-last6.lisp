;;; File: pass-last.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The pass Last World Environment

;;; ===========================================================
;;; This definition should be changed to obtain different testing environment
;;; At least, you will put your agent name instead of ask-user-tl-agent6
;;;
(defstruct (tl-world6 (:include hexagrid-environment
    (size (@ 10 10))                       ; 10 x 10 grid
    (aspec '((AT FREE? ask-user-tl-agent6)  ; agents that will play the game
             (AT FREE? ask-user-tl-agent6-orange) ; the first is trying to catch&pass
	     ;(AT FREE? ask-user-tl-agent6)  ; remaining try to escape him
             ;(AT FREE? ask-user-tl-agent6)
	     ))
    (bspec '((at edge wall)                ; grid border is wall
             (* 5 obstacle)))
    (info-lines 4) ; Should be the number of agents + 1
    )))            ; 5 obstacles at random locations

;;; ============================================================

(defstruct (tl-agent6
                (:include gui-agent (body (make-tl-agent6-body))))
  "An agent for hn world.")

(defstruct (tl-agent6-body (:include agent-body))
  (grid nil)
  (ma-babu? nil))

(defstruct (ask-user-tl-agent6 (:include tl-agent6 (key-input-program 'ask-user-tl)))
  "An agent that asks the user to type in an action.")

; Orange variant of the above ask-user-agent
(defstruct (tl-agent6-body-orange (:include tl-agent6-body (color 'orange))))

(defstruct (ask-user-tl-agent6-orange (:include ask-user-tl-agent6
						(body (make-tl-agent6-body-orange))
						)))

(defun ask-user-tl (percept key)
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
	 'pass)
	)
  )
  ;(let* ((agent-body (car percept))
         ;(ma-babu (if (tl-agent6-body-ma-babu? agent-body) 'ano 'ne)))
    ;(setf percept (cdr percept))
    ;(format t "~&Agent ~A; Ma-babu: ~A Percept is ~A;
;~&action (right midright forw midleft left pass turnleft turnright)? "
       ;agent-body ma-babu percept)
    ;(read)))

;;; ================================================================
;;; This is to be defined when designing a new agent
;
;(defstruct (xxx    ; replace xxx by your unique name, as e.g. FIT username
;                (:include tl-agent6
;                  (body (make-xxx-body))
;                  (program 'xxx_program)))
;  "Your agent for  tl-world6.")
;
;(defstruct (xxx-body
;                (:include tl-agent6-body))
;  (slot1 default1)  ; any specific extra slots your agent's body would need
;  ...
;  (slotn defaultn))
;
;(defun xxx_program (percept)
;  (let ((agent-body (first percept)))    ; extracts agent body from percept
;    (setf percept (second percept))      ; extracts proper percept
; ...
; ...  here your program comes to calculate and return a proper action
; ...
;  ))
;
;;; Any number of auxiliary functions can follow
;;;
;;; To test run the game you perform
;;; (run-environment (make-tl-world6 :max-steps 10)); or more than 10 steps
;;;==================================================================


;;;; Defining the generic functions

(defmethod initialize ((env tl-world6))
  "Called once to do whatever is necessary to set up the environment
  for running the simulation."
  (call-next-method)
  (let* ((agents (environment-agents env))
         (body1 (agent-body (first agents))))
    (setf (tl-agent6-body-ma-babu? body1) T)
    env))

(defmethod update-fn ((env tl-world6))
  (for each agent in (environment-agents env) do
     (if (tl-agent6-body-ma-babu? (agent-body agent))
         (decf (tl-agent6-score agent))
         (incf (tl-agent6-score agent))))
  (let ((baba
         (find-if #'(lambda (x) (tl-agent6-body-ma-babu? (agent-body x)))
                  (environment-agents env))))
    (setf (environment-agents env)
          (cons baba (delete baba (environment-agents env)))))
  (call-next-method))

(defmethod termination? ((env tl-world6))
 (> (environment-step env) (environment-max-steps env)))

(defmethod performance-measure ((env tl-world6) agent)
  "Score plus for not having babu, minus for having babu."
  (tl-agent6-score agent))

(defun agent-body-test (p)
  "Tests whether the agent has a specific body."
  (if (eq (object-name (agent-body p)) (object-name *agent-body*))
    T
    nil
    )
  )

(defmethod create-percept-loc ((env tl-world6) loc heading agent-body reveal-program)
  "Creates percept for one location."
  (let* ((n-agent    (find-object-if #'agent-body-p loc env))
	 (n-ma-babu  (and n-agent (tl-agent6-body-ma-babu? n-agent)))
	 (n-obstacle (find-object-if #'obstacle-p loc env))
	 (n-wall     (and n-obstacle (find-object-if #'wall-p loc env)))
	 (bump (object-bump agent-body))
	 result)
    ; create percept
    (setf result
	  (list (cond
		  (n-ma-babu  (cons 'baba (object-heading n-agent)))
		  (n-agent    (cons 'agent (object-heading n-agent)))
		  (n-wall     'wall)
		  (n-obstacle 'obstacle)))
	  )
    (let* (
	   (visible (node-visibility loc))
	   (color (convert-color
		    (if visible (object-color agent-body) 'grey)))
	   )
      ; find the number of the agent
      (let ((num (if (numberp (object-name agent-body))
		   (object-name agent-body)
		   0)))
	; draw percept box on the screen
	(draw-percept loc env color :hexagon T :shrink (- num 1))
	)
      )
    result
    )
  )

(defmethod get-percept ((env tl-world6) agent)
  "Perceive nil until the first object seen in every direction from where agent is heading."

  (unless (tl-agent6-body-grid (agent-body agent))
    (setf (tl-agent6-body-grid (agent-body agent))
	  (make-array (grid-environment-size env) :initial-element '())))

  (let ((loc (object-loc (agent-body agent)))
	(heading (object-heading (agent-body agent)))
	)
    ; start bfs from one position forward
    (run-percept-bfs (update-loc loc heading) heading env (agent-body agent))
    )
  (setf *percept* (cons (agent-body agent) 
			(if (object-bump (agent-body agent))
			  (list 'bump)
			  *percept*
			  )
			))

  ;(format t "~&Percept is ~A" *percept*)
  *percept*
  )

(defun check-grid-contents (env loc)
  (unless (empty-loc? loc env)
    (let* ((agent-bd (find-object-if #'agent-body-p loc env))
           (baba (and agent-bd (tl-agent6-body-ma-babu? agent-bd)))
           (obstacle (find-object-if #'obstacle-p loc env)))
      (cond (obstacle  'O)
            (agent-bd (list (if baba 'B 'A)
                            (object-heading agent-bd)))))))

(defmethod legal-actions ((env tl-world6))
  "In the hide-seek world, agent can move around and pass."
  '(north northwest northeast pass turnleft turnright stop))

;;;; Actions

(defmethod pass ((env tl-world6) agent-body &optional args)
  "Agent with baba heading to a neighbor agent passes the baba."
  (declare-ignore args) ;; They are used in other environments
  (let* ((loc (object-loc agent-body))
         (heading (object-heading agent-body))
         (neighbor (xy-update loc heading))
         (n-agent-body (find-object-if #'agent-body-p neighbor env)))
     (cond (n-agent-body
            (setf (tl-agent6-body-ma-babu? agent-body) nil
                  (tl-agent6-body-ma-babu? n-agent-body) t))
           (t (setf (environment-illegal-actions env)
                    (nconc (environment-illegal-actions env)
                           (list agent-body 'pass)))))))

(defmethod turnleft ((env tl-world6) agent-body &optional args)
  "Turn left."
  (setf (object-heading agent-body) (tnorthwest (object-heading agent-body))))

(defmethod turnright ((env tl-world6) agent-body &optional args)
  "Turn left."
  (setf (object-heading agent-body) (tnortheast (object-heading agent-body))))

(defmethod stop ((env tl-world6) agent-body &optional args)
  "Stop hs world execution."
  (setf (environment-max-steps env) 0))

(defmethod northwest ((env tl-world6) agent-body &optional (args 1))
  "Go args steps to the northwest from where heading."
   (let ((heading (tnorthwest (copy-list (object-heading agent-body)))))
     (dotimes (i args)
       (move-object-by agent-body heading env))))

(defmethod northeast ((env tl-world6) agent-body &optional (args 1))
  "Go one step to the northeast from where heading."
   (let ((heading (tnortheast (copy-list (object-heading agent-body)))))
     (dotimes (i args)
       (move-object-by agent-body heading env))))

(defmethod north ((env tl-world6) agent-body &optional (args 1))
  "Go args steps to where heading."
   (let ((heading (object-heading agent-body)))
     (dotimes (i args)
       (move-object-by agent-body heading env))))

; (run-gui (make-tl-world6))

