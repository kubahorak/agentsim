;;;
;;; Functions related to graphical user interface and SDL.
;;;

(defun run-gui (env)
  "Simulates an environment and presents it in a graphical user interface."
  (setf base-win-width  640)
  (setf base-win-height 640)
  ; extra space on the left and in the bottom
  (defparameter *win-left-padding*   30)
  (defparameter *win-right-padding*   8)
  (defparameter *win-top-padding*     5)
  (defparameter *win-bottom-padding* (+ 70 (* 20 (hexagrid-environment-info-lines env))))
  (let* ((envsize (hexagrid-environment-size env))
	 (bywidth  (/ base-win-width  (xy-x envsize)))
	 (byheight (/ base-win-height (xy-y envsize)))
	 )
    (defparameter *hexsize* (floor (min bywidth byheight)))
    (setf win-width  (+ (* *hexsize* (/ (+ (* (xy-x envsize) 3) 1) 4))        *win-left-padding* *win-right-padding*))
    (setf win-height (+ (* *hexsize* (/ (+ (* (xy-y envsize) (sqrt 3)) 1) 2)) *win-top-padding*  *win-bottom-padding*))
    )
  ; ignore warnings caused by external library Lispbuilder-SDL
  (handler-bind ((warning #'ignore-warning))
    (sdl:with-init ()
		   (sdl:window (round win-width) (round win-height)
			       :title-caption "Agent simulation"
			       :icon-caption "Agent simulation"
			       ; because we don't want tearing
			       :double-buffer T
			       )
		   (when sdl:*gfx-loaded-p*
		     (sdl:initialise-default-font)
		     (sdl:enable-unicode)
		     )
		   (initialize env)
		   ; display only the snapshot, no extra output to the console
		   (display-environment-snapshot env)
		   ; event loop - wait for new events
		   (sdl:with-events (:wait)
				    ; to be able to quit
				    (:quit-event () t)
				    ; main event logic
				    (:key-down-event (:key key) (key-pressed key env))
				    ; when the screen gets modified by external application
				    (:video-expose-event () (sdl:update-display))
				    ))
    )
  )

(defun draw-board (env &key (key))
  "Draws a main board."

  ; first clear the old screen
  (sdl:clear-display sdl:*black*)

  (let* (
	 (envsize (hexagrid-environment-size env))
	 (max-x (- (xy-x envsize) 1))
	 (max-y (- (xy-y envsize) 1))
	 (array (grid-environment-grid env))
	 )
    (for y1 = 0 to max-y do
	 (let ((y (- max-y y1)))
	   ;; Print each location
	   (for x = 0 to max-x do
		; draw from bottom to the top
		(draw-cell (@ x y1) *hexsize* (aref array x y) :key key)
		)
	   ))
    (when sdl:*gfx-loaded-p*
      (draw-legend max-x max-y)
      (draw-info env max-y)
      )
    )
  )

(defun draw-cell (wxy size objects &key (key))
  "Draws cell at the specified world coordinates."
  (let* (
	; decide on color
	(color
	  (if (eq (mod (xy-x wxy) 2) 0)
	    ; even column has color sequence 3-1-2
	    (case (mod (xy-y wxy) 3)
	      (0 *color-3*)
	      (1 *color-1*)
	      (2 *color-2*))
	    ; odd column has color sequence 1-2-3
	    (case (mod (xy-y wxy) 3)
	      (0 *color-1*)
	      (1 *color-2*)
	      (2 *color-3*))
	    ))
	(sxy (scale-world-to-screen wxy size))
	(x (xy-x sxy))
	(y (xy-y sxy)))

    ; draw background tile
    (draw-hexagon x y size color)
    ; draw objects on the tile - the surfaces are returned by 'key' function
    (if (not (not objects))
      (let ((surface
	      (sdl:create-surface size size :x (round x) :y (round y)
				  :color-key sdl:*magenta*)))
	(sdl:fill-surface sdl:*magenta* :surface surface)
	(funcall key surface objects)
	(sdl:draw-surface surface)
      ))
    ))

(defun scale-world-to-screen (xy size)
  "Transforms coordinates from world to screen."
  (let* (
	 ; calculate absolute graphics position
	 (x (* (xy-x xy) size 0.75))
	 (y (* (xy-y xy) size (/ (sqrt 3) 2)))
	 ; x offset
	 (x (+ x *win-left-padding*))
	 ; y offset
	 (y (+ y *win-top-padding*))
	 )
    ; even column means slightly lower position
    (if (eq (mod (xy-x xy) 2) 0)
      (incf y (* size (/ (sqrt 3) 4))))
    (@ x y)
  ))

(defun draw-percept (worldxy env color &key (circle nil) (hexagon nil) (shrink 0))
  "Draws percept at the specified world coordinates."
  (let* (
	(envsize (hexagrid-environment-size env))
	; drawing from the top, not bottom
	(max-y (- (xy-y envsize) 1))
	(xy1 (@ (xy-x worldxy) (- max-y (xy-y worldxy))))
	(xy  (scale-world-to-screen xy1 *hexsize*))
	)

    (when hexagon
    ; draw 5/6 of the normal size
    (let* ((newsize (/ (* *hexsize* 2) 3))
	   ; minus 5 pixels for one shrink level
	   (newsize (- newsize (* shrink 5)))
	   (x (+ (xy-x xy) (/ (- *hexsize* newsize) 2)))
	   (y (+ (xy-y xy) (/ (- *hexsize* newsize) 2)))
	   )
      ; draw three times, so we get nice 2 point line
      (draw-hexagon x y newsize color :filled nil)
      (incf x)
      (draw-hexagon x y newsize color :filled nil)
      (incf y)
      (draw-hexagon x y newsize color :filled nil)
      )
    )
    (when circle
      (let* ((half (/ *hexsize* 2))
	     (x (+ (xy-x xy) half))
	     (y (+ (xy-y xy) half))
	     (r    (/ *hexsize* 6)))
	(sdl:draw-filled-circle-* (round x) (round y) (round r) :color color)
	)
      )
  ))

(defun draw-hexagon (x y size color &key (surface sdl:*default-display*) (filled T))
  "Draws a hexagon at the specified screen coordinates."
  (let ((vertices nil))
    ; build list of vertices
    ; relative coordinates
    ; [ 0           ; size / 2 ]
    (setf vertices (cons (sdl:point
			   :x (round x)
			   :y (round (+ y (/ size 2)))) vertices))
    ; [ size / 4    ; size * ((2 - sqrt(3))/4) ]
    (setf vertices (cons (sdl:point
			   :x (round (+ x (/ size 4)))
			   :y (round (+ y (* size (/ (- 2 (sqrt 3)) 4))))) vertices))
    ; [ size * 0.75 ; size * ((2 - sqrt(3))/4) ]
    (setf vertices (cons (sdl:point
			   :x (round (+ x (* size 0.75)))
			   :y (round (+ y (* size (/ (- 2 (sqrt 3)) 4))))) vertices))
    ; [ size        ; size / 2 ]
    (setf vertices (cons (sdl:point
			   :x (round (+ x size))
			   :y (round (+ y (/ size 2)))) vertices))
    ; [ size * 0.75 ; size - (size * ((2 - sqrt(3))/4)) ]
    (setf vertices (cons (sdl:point
			   :x (round (+ x (* size 0.75)))
			   :y (round (+ y (- size (* size (/ (- 2 (sqrt 3)) 4)))))) vertices))
    ; [ size / 4    ; size - (size * ((2 - sqrt(3))/4)) ]
    (setf vertices (cons (sdl:point
			   :x (round (+ x (/ size 4)))
			   :y (round (+ y (- size (* size (/ (- 2 (sqrt 3)) 4)))))) vertices))

    ; draw outline
    (if (and sdl:*gfx-loaded-p* filled)
      (sdl:draw-filled-polygon vertices :color color :surface surface)
      (sdl:draw-polygon vertices :color color :surface surface))
    ))

; Main logic here
(defun key-pressed (key env)
  "This function gets called on key-pressed event."
  (when (sdl:key= key :sdl-key-escape)
    (sdl:push-quit-event))
  (let ((steps (get-steps key)))
    (dotimes (i steps)
      (incf (environment-step env))
      ;; Deliver percept and get action from each agent
      (for each agent in (environment-agents env) do
	   (when (null (agent-action agent))
	     (setf (agent-action agent)
		   ; do we have key input program, or an independent agent
		   (if (eq (gui-agent-key-input-program agent) #'nothing)
		     ; independent agent
		     (funcall (agent-program agent) (agent-percept agent))
		     ; pass the input key to the key-input-program
		     (funcall (gui-agent-key-input-program agent) (agent-percept agent) key)
		     )
		   )
	     (when (> (count-remaining-ask-agents env) 0)
	       (return-from key-pressed nil)
	       )
	     )
	   )
      ;; Execute the actions and otherwise update the world
      (update-fn env)
      ;; Update the agent scores, then optionally display the current state
      (for each agent in (environment-agents env) do
	   (setf (agent-score agent) (performance-measure env agent)))
      (display-illegal-actions env)
      (display-environment env)
      (clear-agent-actions env)
      (when (termination? env)
	(sdl:push-quit-event)
	(return)
	)
      )
    )
  )

(defun clear-agent-actions (env)
  "Clears all agent actions."
  (for each agent in (environment-agents env) do
       (setf (agent-action agent) nil)
       )
  )

(defun count-remaining-ask-agents (env)
  "Returns number of ask-user-agents without an action."
  (let ((r 0))
    (for each agent in (environment-agents env) do
	 (when (and (null (agent-action agent))
		    (not (eq (gui-agent-key-input-program agent) #'nothing))
		    )
	   (incf r)
	   )
	 )
    r
    )
  )

; because we need extra program to ask for input
(defstructure (gui-agent (:include agent))
	      "Extends agent by adding program for handling user key input."
	      (key-input-program #'nothing)
	      )

(defun draw-legend (max-x max-y)
  "Draws legend of the board."
  ; draw Y legend
    (for wy = 0 to max-y do
	 (let* ((ty (- max-y wy))
		(sxy (scale-world-to-screen (@ 0 ty) *hexsize*))
		(sx 10)
		; little more down
		(sy (+ (xy-y sxy) (/ *hexsize* 5))))
	   ;; Print each location
	   (sdl:draw-string-solid-* (write-to-string wy) sx (round sy) :color sdl:*white*)
	   )
	 )

    ; draw X legend
    (for wx = 0 to max-x do
	 (let* ((sxy (scale-world-to-screen (@ wx (+ max-y 1)) *hexsize*))
		; little to the right
		(sx (+ (xy-x sxy) (/ *hexsize* 2) (- 5)))
		; little more down
		(sy (+ (xy-y sxy) (/ *hexsize* 3))))
	   ;; Print each location
	   (sdl:draw-string-solid-* (write-to-string wx) (round sx) (round sy) :color sdl:*white*)
	   )
	 )
  )

(defun draw-info (env max-y)
  "Draw info about the environment to the screen."
  (let* ((sxy (scale-world-to-screen (@ 0 (+ max-y 1)) *hexsize*))
	 (sx  (xy-x sxy))
	 (sy  (xy-y sxy))
	 (sy  (+ sy *hexsize*))
	 ; call generic method on the environment to get the texts
	 (strs (info-text-environment env 3))
	 )
    (dolist (line strs)
      (sdl:draw-string-solid-* line (round sx) (round sy) :color sdl:*white*)
      (setf sy (+ sy 20))
      )
    )
  )

;;; Background colors of the board
(defparameter *color-1*          (sdl:color :r  90 :g 209 :b 213))
(defparameter *color-2*          (sdl:color :r  47 :g 196 :b 196))
(defparameter *color-3*          (sdl:color :r  47 :g 196 :b 146))

;;; Extra colors
(defparameter *color-dark-grey*  (sdl:color :r 30  :g 30  :b 30))
(defparameter *color-grey*       (sdl:color :r 128 :g 128 :b 128))
(defparameter *color-orange*     (sdl:color :r 255 :g 140 :b 0))
(defparameter *color-brown*      (sdl:color :r 115 :g 74  :b 18))

(defun convert-color (sym)
  "Converts color from symbol representation to SDL color."
  (case sym
    ('black sdl:*black*)
    ('yellow sdl:*yellow*)
    ('white sdl:*white*)
    ('red sdl:*red*)
    ('green sdl:*green*)
    ('brown *color-brown*)
    ('grey *color-grey*)
    ('dark-grey *color-dark-grey*)
    ('orange *color-orange*)
    )
  )

(defun get-steps (key)
  "Return the number of steps based on the pressed key."
  (let ((steps 1))
    (when (sdl:key= key :sdl-key-2)
      (setf steps 2))
    (when (sdl:key= key :sdl-key-3)
      (setf steps 3))
    (when (sdl:key= key :sdl-key-4)
      (setf steps 4))
    (when (sdl:key= key :sdl-key-5)
      (setf steps 5))
    (when (sdl:key= key :sdl-key-6)
      (setf steps 6))
    (when (sdl:key= key :sdl-key-7)
      (setf steps 7))
    (when (sdl:key= key :sdl-key-8)
      (setf steps 8))
    (when (sdl:key= key :sdl-key-9)
      (setf steps 9))
    (when (sdl:key= key :sdl-key-0)
      (setf steps 10))
    (when (sdl:key= key :sdl-key-h)
      (setf steps 100))
    (when (sdl:key= key :sdl-key-t)
      (setf steps 1000))
    steps
    )
  )
