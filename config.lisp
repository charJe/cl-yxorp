(in-package #:yxorp)

(defstruct (ssl-config (:constructor ssl-config))
  (certificate "cert.pem"
   :type (or pathname string)
   :read-only t)
  (key "key.pem"
   :type (or pathname string)
   :read-only t)
  (password nil
   :type (or string null)
   :read-only t)
  (redirect-port nil
   :type (or integer null)
   :read-only t)
  (redirect-destination 443
   :type integer
   :read-only t))

(defstruct (config (:constructor config))
  (port 8080
   :type integer
   :read-only t)
  (destinator (lambda () 8081)
   :type (function () (or string number null))
   :read-only t)
  (request-filter (lambda (body) body)
   :type (function (string) string)
   :read-only t)
  (response-filter (lambda (body) body)
   :type (function (string) string)
   :read-only t)
  (ssl nil
   :type (or ssl-config null)
   :read-only t))

(defun packageize (string)
  (str:concat "(progn #.(in-package #:yxorp-config) " string ")"))

(defun read-config (file)
  (-> file
    uiop:read-file-string
    packageize
    read-from-string
    eval))
