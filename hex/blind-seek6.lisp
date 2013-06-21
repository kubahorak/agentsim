;;; File: blind-seek.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The Blind-Seek World Environment

(defparameter *dl* '((0 1) (1 1) (1 0) (0 -1) (-1 0) (-1 1)))

;;; ===========================================================
;;; This definition should be changed to obtain different testing environment
;;; At least, you will put your agent name instead of bs-agent6
;;;
(defstructure (bs-world6 (:include hexagrid-environment
    (size (@ 10 10))               ; 10 x 10 grid
    (aspec '((at free? ask-user-bs-agent6))) ; one bs-agent6 at a random free location
    (bspec '((at edge wall)        ; grid edge locations contain wall
             (* 10 obstacle)       ; 20 obstacles at random locations
             (* 5 (at free? person)))))  ; five persons at random free locations
    ))
;;; ===========================================================

(defstructure (bs-agent6
                (:include gui-agent (body (make-bs-agent6-body))))
  "An agent for bs world.")

(defstructure (bs-agent6-body (:include agent-body))
  (penalty nil)
  (grid (make-array (@ 10 10) :initial-element '())))

(defstructure (ask-user-bs-agent6 (:include bs-agent6 (key-input-program 'ask-user-bs)))
  "An agent that asks the user to type in an action.")


(defun ask-user-bs (percept key)
  "Let user decide, what action to take, by pressing a key."
  (cond ((sdl:key= key :sdl-key-q)
	 'northwest)
	((sdl:key= key :sdl-key-w)
	 'north)
	((sdl:key= key :sdl-key-e)
	 'northeast)
	((sdl:key= key :sdl-key-a)
	 'southwest)
	((sdl:key= key :sdl-key-s)
	 'south)
	((sdl:key= key :sdl-key-d)
	 'southeast)
	((sdl:key= key :sdl-key-x)
	 'stop)
	)
  )


;;; ================================================================
;;; This is to be defined when designing a new agent
;
(defstructure (xxx    ; replace xxx by your unique name, as e.g. FIT username
                (:include bs-agent6
                  (body (make-xxx-body))
                  (program 'xxx_program)))
  "AnYour agent for bs world.")
;
(defstructure (xxx-body
                (:include bs-agent6-body))
;  (slot1 default1)  ; any specific extra slots your agent's body would need
;  ...
;  (slotn defaultn))
)
(defun xxx_program (percept)
  (let ((agent-body (first percept)))    ; extracts agent body from percept
    (setf percept (second percept))      ; extracts proper percept
    'north
; ...
; ...  here your program comes to calculate and return a proper action
; ...
) )
;;; Any number of auxiliary functions can follow
;;;
;;; To test run the game you perform
;;; (run-environment (make-bs-world6 :max-steps 10)); or more than 10 steps
;;; ==================================================================


(defun rand-heading ()
  (copy-list (nth (random 6) *dl*)))

;;;; Defining the generic functions

(defmethod initialize ((env bs-world6))
  "Called once to do whatever is necessary to set up the environment
  for running the simulation."
  (call-next-method)
  (let* ((agent (first (environment-agents env)))
         (body (agent-body agent))
         (grid (grid-environment-grid env))
         (agrid (bs-agent6-body-grid body)))
    (move-object-to body (random-loc env) env)
    (setf (object-bump body) nil)
    (dotimes (x (xy-x (grid-environment-size env)))
      (dotimes (y (xy-y (grid-environment-size env)))
        (let ((gxy (aref grid x y)))
          (setf (aref agrid x y) gxy)
          (when (member-if #'bs-agent6-body-p gxy)
            (setf (aref agrid x y) nil)))))
    grid))

(defmethod update-fn ((env bs-world6))
  (for each agent in (environment-agents env) do
    (decf (bs-agent6-score agent)))
  (call-next-method))

(defmethod termination? ((env bs-world6))
  "End when some person met by agent or stop action performed."
  (> (environment-step env) (environment-max-steps env)))

(defmethod performance-measure ((env bs-world6) agent)
  "Agent score is taken."
  (if (bs-agent6-body-penalty (agent-body agent)) -1000
      (bs-agent6-score agent)))

(defmethod get-percept ((env bs-world6) agent)
  "Perceive bump or nil."
  (let ((abody (agent-body agent)))
    (list abody (if (object-bump abody) 'bump nil))))

(defmethod legal-actions ((env bs-world6))
  "In the wumpus world, agents can move around, grab gold and shoot arrows."
  '(north northwest northeast south southwest southeast stop))

;;;; Actions

(defmethod northwest ((env bs-world6) agent-body &optional (args 1))
  "Go one step to the northwest from where heading."
   (let ((heading (tnorthwest (copy-list (object-heading agent-body)))))
     (move-object-by agent-body heading env))
   (let ((loc (object-loc agent-body)))
     (when (find-object-if #'person-p loc env)
       (setf (environment-max-steps env) 0))))

(defmethod northeast ((env bs-world6) agent-body &optional (args 1))
  "Go one step to the northeast from where heading."
   (let ((heading (tnortheast (copy-list (object-heading agent-body)))))
     (move-object-by agent-body heading env))
   (let ((loc (object-loc agent-body)))
     (when (find-object-if #'person-p loc env)
       (setf (environment-max-steps env) 0))))

(defmethod southwest ((env bs-world6) agent-body &optional (args 1))
  "Go one step to the southwest from where heading."
   (let ((heading (tsouthwest (copy-list (object-heading agent-body)))))
     (move-object-by agent-body heading env))
   (let ((loc (object-loc agent-body)))
     (when (find-object-if #'person-p loc env)
       (setf (environment-max-steps env) 0))))

(defmethod southeast ((env bs-world6) agent-body &optional (args 1))
  "Go one step to the southeast from where heading."
   (let ((heading (tsoutheast (copy-list (object-heading agent-body)))))
     (move-object-by agent-body heading env))
   (let ((loc (object-loc agent-body)))
     (when (find-object-if #'person-p loc env)
       (setf (environment-max-steps env) 0))))

(defmethod north ((env bs-world6) agent-body &optional (args 1))
  "Go one step to where heading."
   (let ((heading (object-heading agent-body)))
     (move-object-by agent-body heading env))
   (let ((loc (object-loc agent-body)))
     (when (find-object-if #'person-p loc env)
       (setf (environment-max-steps env) 0))))

(defmethod south ((env bs-world6) agent-body &optional (args 1))
  "Go one step opposite to where heading."
   (let ((heading (tsouth (copy-list (object-heading agent-body)))))
     (move-object-by agent-body heading env))
   (let ((loc (object-loc agent-body)))
     (when (find-object-if #'person-p loc env)
       (setf (environment-max-steps env) 0))))

(defmethod stop ((env bs-world6) agent-body &optional (args 1))
  "Stop bs world execution and check if persons inaccessible."
  (setf (environment-max-steps env) 0)
  (if (persons-accessible (bs-agent6-body-grid agent-body)
                          (object-loc agent-body))
      (setf (bs-agent6-body-penalty agent-body) -1000)))

(defun persons-accessible (grid location)
  (let ((open (list location)) loc x y val)
    (loop
      (if (null open) (return nil))
      (setf loc (first open)
            open (cdr open)
            x (xy-x loc)
            y (xy-y loc)
            val (aref grid x y))
      (when (find-if #'person-p val) (return-from persons-accessible T))
      (setf (aref grid x y) 'C)
      (setf open (append open
                         (new-locs loc grid open (array-dimensions grid)))))))

(defun new-locs (loc grid open g-size)
  (let ((nl (mapcar #'(lambda (x) (xy-update loc x))
                    *dl*)))
    (remove-if #'(lambda (new-loc)
                   (or (not (inside new-loc (xy-x g-size) (xy-y g-size)))
                       (let ((val (aref grid (xy-x new-loc) (xy-y new-loc))))
                         (or (eq val 'C)
                             (find-if #'obstacle-p val)
                             (member new-loc open :test #'equal)))))
               nl)))

; (run-environment (make-bs-world6 :max-steps 10))

