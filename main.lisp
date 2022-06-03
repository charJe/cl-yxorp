(in-package #:yxorp)

(defvar *suppress-errors* t)

(defmacro with-socket-handler-case (stream &body body)
  `(block nil
     (handler-bind
         ((cl+ssl::ssl-error-ssl
            (lambda (c) (declare (ignore c))
              (format *error-output* "Non-ssl connection attempt; Consider configuring the ssl-redirect server.~%")
              (when *suppress-errors* (return))))
          (stream-error
            (lambda (condition)
              (close ,stream :abort t)
              (format *error-output* "Client unexpectedly disconnected with ~A.~%"
                      (type-of condition))
              (when *suppress-errors* (return))))
          (error (lambda (c)
                   (format *error-output* "~A~%" c)
                   (when *suppress-errors* (return)))))
       ,@body)))

(defun websocket-handler (client server)
  (track-thread
   (bt:make-thread
    (lambda ()
      (unwind-protect
           (forward-stream server client)
        (handler-case (close client)
          (stream-error nil))))
    :name "YXORP Server->Client"))
  (write-headers server)
  (forward-stream client server))

(defun http-handler (client server config)
  (let ((body (read-body client (config-request-filter config))))
    (write-body-and-headers body server))
  (let* ((*headers* (alist->ht (parse-response-headers server)))
         (*response-headers* *headers*)
         (body (read-body server (config-response-filter config))))
    (write-body-and-headers body client)))

(defun filter-encodings (headers)
  (let* ((*headers* headers))
    (setf (header :accept-encoding)
          (->> :accept-encoding
            extract-encodings-from
            (remove-if-not #'encodingp)
            (mapcar #'symbol-name)
            (mapcar #'str:downcase)
            (str:join ", ")))
    *headers*))

(defun save-ip (headers)
  (if (find "FORWARDED"
            headers
            :key (lambda (cons)
                   (symbol-name (car cons)))
            :test 'str:containsp)
      headers
      (let ((ip (str:join "." (map 'list 'princ-to-string usocket:*remote-host*))))
        (append
         (list (cons :x-forwarded-for ip))
         (list (cons :forwarded
                     (str:concat "for=" ip)))
         headers))))

(defclass server ()
  ((conifg :type config :initarg :config :reader server-config
           :initform (alexandria:required-argument "config"))
   (connections :initform (list) :accessor server-connections)
   adder
   workers
   (add-delete-lock :initform (bt:make-lock) :accessor add-delete-lock)))

(defclass connection ()
  ((lock :initform (bt:make-lock) :accessor connection-lock)
   (source :initarg :client :type usocket:socket :reader source-socket)
   (source-stream :type stream :accessor source-stream)
   (destination :initarg :client :type usocket:socket :reader destination-socket)
   (destination-stream :type (or null stream) :accessor destination-stream)
   (buffer :initform (smart-buffer:make-smart-buffer)
           :accessor connection-buffer)
   (header-mode :initform t :reader header-mode-p)
   (previous-byte2 :initform 0)
   (previous-byte :initform 0)
   (gc :initform nil :type boolean :accessor connection-gc)))

(defclass client-connection (connection) ())
(defclass server-connection (connection) ())

(defmethod body-mode-p ((obj connection))
  (not (header-mode-p obj)))

(defmethod (setf previous-byte) (byte (obj connection))
  (setf (slot-value obj 'previous-byte2) (slot-value obj 'previous-byte)
        (slot-value obj 'previous-byte) byte))

(defmethod headers-done-p ((obj connection))
  (ignore-errors
   (and (= (slot-value obj 'previous-byte2)
           (char-code #\return))
        (= (slot-value obj 'previous-byte)
            (char-code #\newline)))))

(defmethod source-stream ((obj connection))
  (usocket:socket-stream (source-socket obj)))

(defmethod destination-stream ((obj connection))
  (usocket:socket-stream (destination-socket obj)))

(defmethod add-thread-handler ((client usocket:usocket) (obj server))
  (bt:with-lock-held ((add-delete-lock obj))
    (push (make-instance 'client-connection :client client)
          (server-connections obj))))

(defmacro with-lock-held-no-wait ((place) &body body)
  (let ((lock (gensym)))
    `(let ((,lock ,place))
       (when (bt:acquire-lock ,lock nil)
         (unwind-protect ,@body
           (bt:release-lock ,lock))))))

(defmethod header-mode ((obj connection))
  (setf (connection-buffer obj) (smart-buffer:make-smart-buffer)
        (previous-byte obj) 0
        (slot-value obj 'destination) nil
        (destination-stream obj) nil
        (slot-value obj 'header-mode) t))

(defmethod body-mode ((obj connection))
  (setf (connection-buffer obj) (smart-buffer:make-smart-buffer)
        (previous-byte obj) 0
        (source-stream obj) (apply-encodings (source-stream obj) )
        (slot-value obj 'destination) nil
        (destination-stream obj) nil
        (slot-value obj 'header-mode) nil))

(defmethod work ((obj server))
  (loop for connections = (server-connections obj) do
    (loop for connection in connections do
      (with-lock-held-no-wait ((connection-lock connection))
        (unless (connection-gc connection)
          (handler-case
              (loop with buffer = (make-instance 'smart-buffer-stream
                                                 :buffer (connection-buffer connection))
                    with input = (source-stream connection)
                    while (listen input)
                    for byte = (read-byte input)
                    while byte do
                      (setf (previous-byte connection) byte)
                      (write-byte byte buffer)
                      (cond
                        ((and (header-mode-p connection)
                              (headers-done-p connection))
                         (body-mode connection))
                        ((and (body-mode-p connection)
                              (body-done-p connection))
                         (send connection (server-config obj))
                         (typecase connection
                           (server-connection
                            (setf (connection-gc connection) t))
                           (client-connection
                            (header-mode connection))))))
            (stream-error ()
              (setf (connection-gc connection) t))))))))

(defmethod make-worker ((obj server))
  (lambda () (work obj)))

(defun event-loop (port handler arguments)
  (bt:make-thread
   (lambda ()
     (let* ((element-type '(unsigned-byte 8))
            (socket (usocket:socket-listen
                     usocket:*wildcard-host* port
                     :element-type element-type)))
       (unwind-protect
            (loop
              (let ((client-socket (usocket:socket-accept
                                    socket :element-type element-type)))
                (apply handler client-socket arguments)))
         (usocket:socket-close socket))))
   :name "YXORP Server"))

(defun start (config)
  (let ((config
          (cond ((stringp config) (read-config config))
                ((config-p config) config)
                (t (config)))))
    (handler-case
        (let ((server (make-instance 'server)))
          (prog1 server
            (setf (slot-value server 'adder)
                  (event-loop (config-port config)
                   #'add-thread-handler (list server))
                  (slot-value server 'workers)
                  (loop repeat (config-workers config)
                        collect
                        (bt:make-thread (make-worker server)
                         :name "YXORP Worker")))))
      (address-in-use-error ()
        (format *error-output* "Port ~A is already being used by another program. Edit your configuration to use a different port or stop the other program.~%"
                (config-port config))))))

(defmethod clean-up ((obj connection))
  (close (source-stream obj) :abort t)
  (usocket:socket-close (source-socket obj)))

(defmethod stop ((obj server))
  (flet ((destory (thread)
           (when (bt:thread-alive-p thread)
             (bt:destroy-thread thread))))
    (destory (slot-value obj 'adder))
    (mapc #'destory (slot-value obj 'workers))
    (mapc #'clean-up (server-connections obj))
    (setf (server-connections obj) (list))))

(defun main (&aux (args (uiop:command-line-arguments)))
  (start (nth 0 args))
  ;; this is done in favor of :in-new-thread nil to properly catch interrupt
  (handler-case
      (let ((server (find "YXORP Server" (the list (bt:all-threads))
                          :key 'bt:thread-name
                          :test 'string=)))
        (when server
          (bt:join-thread server)))
    (#+sbcl sb-sys:interactive-interrupt
     #+clozure ccl:interrupt-signal-condition
     #+ecl interrupt-signal-condition
     () (uiop:quit))))
