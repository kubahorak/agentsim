;;;
;;; Load all necessary project files.
;;;

(load "base/utilities.lisp")
(load "base/binary-tree.lisp")
(load "base/queue.lisp")
(load "base/cltl2.lisp")
(load "base/basic-env.lisp")
(load "base/grid-env.lisp")
(load "base/agent.lisp")
(load "base/grid.lisp")
(load "hex/utilities.lisp")
(load "hex/gui.lisp")
(load "hex/percept.lisp")
; hexagrid-env redefines a lot of stuff from grid-env
(handler-bind ((warning #'ignore-warning))
  (load "hex/hexagrid-env.lisp"))
;;; Game files
(load "hex/blind-seek6.lisp")
(load "hex/hide-seek6.lisp")
(load "hex/touch-last6.lisp")
