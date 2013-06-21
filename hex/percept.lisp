;;;
;;; Functions related to creation of percept.
;;;

; similar to XY but with distance and visibility
(defstruct (node (:type list))
  x y distance visibility)

(defun node-add (p q &optional (d 0) (v T))
  "Add two nodes, component-wise, while retaining distance."
  (make-node :x (+ (node-x p) (node-x q)) :y (+ (node-y p) (node-y q)) :distance d :visibility v))

(defun node-equal (p q)
  "Test two nodes for equality of coordinates."
  (and (eq (node-x p) (node-x q)) (eq (node-y p) (node-y q)))
  )

(defun enqueue (queue item)
  "Add node to the BFS queue."
  (cond ((member item *open* :test #'node-equal) queue)
	((member item *closed* :test #'node-equal) queue)
	((member item queue :test #'node-equal) queue)
	(t (cons item queue)))
  )

(defun run-percept-bfs (start heading env agent-body &key (reveal-program #'nothing))
  "Starts the BFS percept algorithm on the map."
  (setf (node-distance start) 0)
  (setf (node-visibility start) T)
  (defparameter *open* (list start))
  (defparameter *closed* nil)
  (defparameter *start* start)
  (defparameter *heading* heading)
  (defparameter *env* env)
  (defparameter *agent-body* agent-body)
  (defparameter *visible-in-row* 0)
  (defparameter *percept-row* '())
  (defparameter *percept* '())
  (defparameter *depth* 0)
  (defparameter *finished* nil)
  (defparameter *reveal-program* reveal-program)
  (percept-breadth-first)
  )

(defun percept-breadth-first ()
  "One step of the BFS percept algorithm."
  (if (or (null *open*) *finished*)
    (return-from percept-breadth-first nil)
    (let ((loc (first *open*)))
      (setf *closed* (cons loc *closed*))
      (setf *open* 
	    (append (rest *open*)
		    (in-location loc *heading*)))
      (percept-breadth-first))))

(defun in-location (loc heading)
  "Actions to do inside location. Returns new locations to open."
  ; check if the location is valid
  ; else return nil
  (if (not (is-valid-percept-loc loc))
    (return-from in-location))

  ; starting to process new row
  (when (< *depth* (node-distance loc))
    (setf *depth* (node-distance loc))
    (setf *percept* (append *percept* (list *percept-row*)))
    (setf *percept-row* '())
    ; end condition
    (when (eq *visible-in-row* 0)
      (setf *finished* T)
      (return-from in-location nil)
      )
    (setf *visible-in-row* 0)
    )

  (let ((result nil))
    ; is the location inside the map?
    (if (and 
	  (and (>= (node-x loc) 0) (>= (node-y loc) 0))
	  (and (<  (node-x loc) 10) (< (node-y loc) 10))
	  )
      ; check object in the location
      (setf result (create-percept-loc *env* loc *heading* *agent-body* *reveal-program*))
      ; behind the walls automatically set result to DARK
      (setf result (list 'DARK))
      )

    ; save percept
    (setf *percept-row* (append *percept-row*
				(if (node-visibility loc)
				  result
				  (list 'DARK))))


    (let ((next-visible (and (node-visibility loc) (null (first result)))))
      (when next-visible
	(incf *visible-in-row*))
      ; open locations N-W, N, N-E
      (let ((locnw (update-loc loc (tnorthwest (copy-list heading))))
	    (locno (update-loc loc heading))
	    (locne (update-loc loc (tnortheast (copy-list heading))))
	    (queue nil)
	    )
	(setf (node-distance locnw) (+ (node-distance loc) 1))
	(setf (node-distance locno) (+ (node-distance loc) 1))
	(setf (node-distance locne) (+ (node-distance loc) 1))
	; set next visibility
	(setf (node-visibility locnw) next-visible)
	(setf (node-visibility locno) next-visible)
	(setf (node-visibility locne) next-visible)
	; build result queue from the right
	(setf queue (enqueue queue locne))
	(setf queue (enqueue queue locno))
	(setf queue (enqueue queue locnw))
	)
      ))
  )

; this function can select locations in the percept
(defun is-valid-percept-loc (loc)
  "Returns T when the location is a valid perception location."
  T
  )

(defun update-loc (loc heading)
  "Move from location in direction of heading."
  (let ((hexx (node-x heading))
	(hexy (node-y heading))
	(objx (node-x loc))
	(objy (node-y loc))
	(dist (node-distance loc))
	(visi (node-visibility loc)))
    (if (not (eq hexx 0)) ; only when moving sideways
      (if (eq (mod objx 2) 0) ; even column
	 (decf hexy 1)
       )
      )
    (node-add (make-node :x hexx :y hexy) loc dist visi)
    ))
