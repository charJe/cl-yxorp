(in-package #:asdf-user)

(defsystem yxorp
  :description "A reverse proxy server that supports WebSocket, HTTP, HTTPS,
HTTP to HTTPS redirecting, port and host forwarding configuration using a real
programming language, HTTP header and body manipulation (also using a real
programming language)."
  :version "1.0"
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :licence "AGPL3"
  :depends-on
  (#:binding-arrows
   #:chipz
   #:chunga
   #:cl+ssl
   #:flexi-streams
   #:rutils
   #:salza2
   #:smart-buffer
   #:str
   #:trivial-garbage
   #:usocket
   #:usocket-server)
  :serial t
  :components
  ((:file "package")
   (:file "smart-buffer-stream")
   (:file "threads")
   (:file "coding")
   (:file "config")
   (:file "utils")
   (:file "main")))
