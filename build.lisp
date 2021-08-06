(push :yxorp-build *features*)

(require 'yxorp)

(declaim
 (optimize
  (compilation-speed 0)
  (debug 0)
  (safety 2)
  (space 2)
  (speed 3)))

(sb-ext:save-lisp-and-die
 "cl-yxorp"
 :toplevel #'yxorp::main
 :executable t
 :compression t)
