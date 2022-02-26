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
     (values "127.0.0.1" destination))
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
   :type (or (function () destination) symbol)
   :read-only t)
  (request-filter (lambda (body) body)
   :type (or (function (string) string) symbol)
   :read-only t)
  (response-filter (lambda (body) body)
   :type (or (function (string) string) symbol)
   :read-only t)
  (ssl nil
   :type (or ssl-config null)
   :read-only t))

(defun read-config (file)
  (flet ((packageize (string)
           (str:concat "(progn #.(in-package #:yxorp-config) "
                       string ")")))
    (let ((package *package*))
      (prog1 (handler-case
                 (-> file
                   uiop:read-file-string
                   packageize
                   read-from-string
                   eval)
               (error (condition)
                 (format *error-output* "There is a problem with your config file:~%~A~%"
                         condition)))
        (setq *package* package)))))
