(in-package #:yxorp)

(deftype port ()
  'integer)

(deftype secure-protocol ()
  '(member :https :wss))

(deftype protocol ()
  '(or
    (member :http :ws)
    secure-protocol))

(deftype destination-specifier ()
  '(or string port))

(defclass destination ()
  ((protocol
    :reader protocol
    :initarg :protocol
    :initform :http
    :type protocol)
   (host
    :reader host
    :initarg :host
    :initform "127.0.0.1"
    :type string)
   (port
    :reader port
    :initarg :port
    :type (or null port))))

(defmethod print-object ((obj destination) out)
  (format out "~(~A~)://~A:~A"
          (protocol obj)
          (host obj)
          (port obj)))

(defun default-protocol-port (protocol)
  (case protocol
    ((:https :wss) 443)
    (otherwise 80)))

(defun make-destination (destination-specifier)
  (if (typep destination-specifier 'port)
      (make-instance 'destination :port destination-specifier)
      (flet ((make-destination-with-protocol (protocol specifier)
               (let ((destination-parts (str:split-omit-nulls ":" specifier)))
                 (case (length destination-parts)
                   (1  (make-instance
                        'destination
                        :protocol protocol
                        :host (first destination-parts)
                        :port (default-protocol-port protocol)))
                   (2 (let ((port (ignore-errors
                                   (parse-integer (second destination-parts)))))
                        (when port
                          (make-instance
                           'destination
                           :protocol protocol
                           :host (first destination-parts)
                           :port port))))))))
        (let ((protocol-parts (str:split-omit-nulls "://" destination-specifier)))
          (if (= 1 (length protocol-parts))
              (make-destination-with-protocol
               :http (first protocol-parts))
              (make-destination-with-protocol
               (make-keyword (first protocol-parts))
               (second protocol-parts)))))))

(defun destination-secure-p (destination)
  (typep (protocol destination)
         'secure-protocol))

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
   :type (or (function () destination-specifier) symbol)
   :read-only t)
  (request-filter nil
   :read-only t)
  (response-filter nil
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
