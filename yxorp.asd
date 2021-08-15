(in-package #:asdf-user)

(defsystem yxorp
  :description "A reverse proxy server that supports WebSocket, HTTP, HTTPS,
HTTP to HTTPS redirecting, port and host forwarding configuration using a real
programming language, HTTP header and body manipulation (also using a real
programming language)."
  :version "0"
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :licence "AGPL3"
  :depends-on
  (#:str
   #:usocket
   #:usocket-server
   #:cl+ssl
   #:binding-arrows
   #:flexi-streams
   #:trivial-garbage)
  :components
  ((:file "package")
   (:file "threads")
   (:file "utils")
   (:file "config")
   (:file "main")))
