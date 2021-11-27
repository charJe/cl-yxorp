(in-package #:asdf-user)

(defsystem yxorp
  :description "A reverse proxy server that supports WebSocket, HTTP, HTTPS,
HTTP to HTTPS redirecting, port and host forwarding configuration using a real
programming language, HTTP header and body manipulation (also using a real
programming language)."
  :version "0.3"
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :licence "AGPL3"
  :depends-on
  (#:binding-arrows
   #:chipz
   #:chunga
   #:cl+ssl
   #:flexi-streams
   #:salza2
   #:str
   #:trivial-garbage
   #:usocket
   #:usocket-server)
  :serial t
  :components
  ((:file "package")
   (:file "threads")
   (:file "coding")
   (:file "utils")
   (:file "config")
   (:file "main")))
