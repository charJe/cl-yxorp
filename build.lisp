(declaim
 (optimize
  (compilation-speed 0)
  (debug 0)
  (safety 2)
  (space 3)
  (speed 3)))

(require :asdf)

(#+quicklisp ql:quickload
 #-quicklisp require 'yxorp)

#+sbcl
(sb-ext:save-lisp-and-die
 "cl-yxorp"
 :toplevel #'yxorp::main
 :executable t
 :compression t)
