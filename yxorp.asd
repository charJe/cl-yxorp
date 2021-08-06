(in-package #:asdf-user)

(defsystem yxorp
  :depends-on
  (#:str
   #:usocket
   #:usocket-server
   #:cl+ssl
   #:binding-arrows
   #:flexi-streams)
  :components
  ((:file "package")
   (:file "utils")
   (:file "config")
   (:file "main")))
