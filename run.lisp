; Change this to your project root directory. Backslash must be doubled (\\).
(cd "~/prg/lisp/agenti/agentsim")

; DONT CHANGE ANYTHING BELOW THIS LINE

(defvar my-quicklisp-dir
  (merge-pathnames (make-pathname :directory '(:relative ".quicklisp"))
                   (user-homedir-pathname)))
(defvar my-setup-file (merge-pathnames my-quicklisp-dir "setup.lisp"))

(load "quicklisp.lisp")

(if (probe-file my-setup-file)
  ; quicklisp already installed
  (load my-setup-file)
  ; quicklisp not installed
  (quicklisp-quickstart:install :path my-quicklisp-dir)
  )

(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(load "loadfiles.lisp")
