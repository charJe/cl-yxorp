(in-package #:yxorp)

(deftype port ()
  'integer)

(deftype destination ()
  '(or string port null))

(defun valid-destination-p (destination)
  (typecase destination
    (null nil)
    (port t)
    (string
     (handler-case
         (-<> destination
           (str:split ":" <> :omit-nulls t)
           second
           parse-integer)
       (parse-error nil)))))

(defun destination-parts (destination)
  (typecase destination
    (port
     (values "localhost" destination))
    (string
     (let ((parts (str:split ":" destination :omit-nulls t)))
       (values (first parts)
               (parse-integer (second parts)))))))

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
   :type (or port null)
   :read-only t)
  (redirect-to 443
   :type port
   :read-only t))

(defstruct (config (:constructor config))
  (port 8080
   :type port
   :read-only t)
  (destinator (lambda () 8081)
   :type (function () destination)
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
