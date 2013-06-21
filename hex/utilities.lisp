;;;
;;; Help functions and other utilities.
;;;

;;; Functions to turn the heading in a specified direction.

(defun tnorthwest (xy)
  "Turn to the north-west from where heading."
  (let ((x (xy-x xy)) (y (xy-y xy)))
    (setf (xy-x xy) (case x
		      (-1 (- y))
		      (0 (- y))
		      (1 (if (zerop y) 1 0)))
	  (xy-y xy) (case y
		      (-1 x)
		      (0 x)
		      (1 (if (eq x -1) 0 1))))
    xy))

(defun tnortheast (xy)
  "Turn to the north-east from where heading."
  (let ((x (xy-x xy)) (y (xy-y xy)))
    (setf (xy-x xy) (case x
		      (-1 (if (zerop y) -1 0))
		      (0 y)
		      (1 y))
	  (xy-y xy) (case y
		      (-1 0)
		      (0 (- x))
		      (1 (if (eq x 1) 0 1))))
    xy))

(defun tsouthwest (xy)
  "Turn to the south-west from where heading."
  (tnorthwest (tnorthwest xy)))

(defun tsoutheast (xy)
  "Turn to the south-east from where heading."
  (tnortheast (tnortheast xy)))

(defun tsouth (xy)
  "Turn to the south from where heading."
  (tnorthwest (tnorthwest (tnorthwest xy))))

;;; Function to move from the location in a specified heading.

(defun xy-update (loc heading)
  "Update XY location with the specified heading."
  (let ((hexx (xy-x heading))
	(hexy (xy-y heading))
	(objx (xy-x loc))
	(objy (xy-y loc)))
    (if (not (eq hexx 0)) ; only when moving sideways
      (if (eq (mod objx 2) 0) ; even column
	(decf hexy 1)
	)
      )
    (xy-add (make-xy :x hexx :y hexy) loc)
    )
  )

(defun is-heading-to (object location)
  "Test if object in its actual position is heading to a neighbouring location."
  (let ((loc (object-loc object))
        (heading (object-heading object)))
    (xy-equal location (xy-update loc heading))))


;;; Hide&Seek percept help functions

(defun count-persons (percept)
  "Counts persons in the percept."
  (let ((p 0))
    (dolist (row percept)
      (dolist (item row)
	(when (eq item 'PERSON)
	  (incf p)
	  )
	)
      )
    p
    )
  )

(defun update-grid (grid percept loc heading)
  "Updates internal grid."
  (when (null grid)
    ; initialize the grid
    (setf grid (make-array (@ 10 10) :initial-element 'DARK))
    ; set walls
    (dotimes (i 10)
      (setf (aref grid i 0) 'WALL)
      (setf (aref grid i 9) 'WALL)
      (setf (aref grid 0 i) 'WALL)
      (setf (aref grid 9 i) 'WALL)
      )
    )

  ; mark current position as nil
  (setf (aref grid (xy-x loc) (xy-y loc)) nil)

  ; parse the percept
  ; yloc goes only nort-west
  ; xloc goes only east
  (let ((yloc loc)
	(rowi 1))
    ; first go to the north
    (setf yloc (xy-update yloc heading))
    ; write percept
    (update-grid-loc grid yloc (first (first percept)))
    ; now branch
    (dolist (row (rest percept))
      ; go north-west
      (setf yloc (xy-update yloc (tnorthwest (copy-list heading))))
      (let ((coli 0)
	    (xloc yloc)
	    )
	; parse row
	(dolist (item row)
	  ; write loc contents to grid
	  (update-grid-loc grid xloc item)
	  ; go east
	  (if (< coli rowi)
	    (setf xloc (xy-update xloc (tnortheast (copy-list heading))))
	    (setf xloc (xy-update xloc (tsoutheast (copy-list heading))))
	    )
	  (incf coli)
	  )
	)
      (incf rowi)
      )
    )
  grid
  )

(defun update-grid-loc (grid loc item)
  "Updates the value in the specified location of the grid."
  (let ((locx (xy-x loc))
	(locy (xy-y loc)))
    (if (and 
	  (and (>= locx 0) (>= locy 0))
	  (and (<  locx 10) (< locy 10))
	  ; overwrite only DARK or (BUSH with PERSON)
	  (or (eq (aref grid locx locy) 'DARK)
	      (and (eq (aref grid locx locy) 'BUSH)
		   (eq item 'PERSON)
		   )
	      )
	  )
      (setf (aref grid locx locy) item)
      )
    )
  )

;;; General utilities

(defun ignore-warning (condition)
  "Function to ignore warnings for the specified conditions."
  (declare (ignore condition))
  (muffle-warning))

