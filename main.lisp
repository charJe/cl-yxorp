(in-package #:yxorp)

(defparameter *external-format* :utf8)

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
   (adder :initform nil)
   (workers :initform nil)
   (add-delete-lock :initform (bt:make-lock) :accessor add-delete-lock)))

(defclass connection ()
  ((lock :initform (bt:make-lock) :accessor connection-lock)
   (source :initarg :source-socket :type usocket:socket :reader source-socket)
   (source-stream :type stream :reader source-stream)
   (destination :initarg :destination-socket
                :type usocket:socket :reader destination-socket)
   (destination-stream :type stream :reader destination-stream)
   (buffer :initform (smart-buffer:make-smart-buffer)
           :accessor connection-buffer)
   (buffer-stream :type smart-buffer-stream :accessor buffer-stream)
   (headers :accessor headers :accessor request-headers)
   (header-mode :initform t :reader header-mode-p)
   (previous-char :initform #\0)
   (previous-char2 :initform #\0)
   (previous-char3 :initform #\0)
   (previous-char4 :initform #\0)
   (gc :initform nil :type boolean :accessor garbagep)))

(defclass client-connection (connection) ())

(defclass server-connection (connection)
  ((headers :accessor response-headers)
   (request-headers :initarg :request-headers
                    :accessor request-headers)))

(defmethod response-headers ((obj client-connection))
  nil)

(defmethod initialize-instance :after ((instance connection) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (header-mode instance)
  (when (slot-boundp instance 'destination)
    (setf (destination-stream instance) (socket-stream (destination-socket instance)))))

(defmethod (setf source-stream) (stream (obj connection))
  (setf (slot-value obj 'source-stream)
        (flex:make-flexi-stream
         (apply-encodings stream nil #|add encodings later|#)
         :external-format *external-format*)))

(defmethod (setf destination-stream) (stream (obj connection))
  (setf (slot-value obj 'destination-stream)
        (flex:make-flexi-stream
         (apply-encodings stream nil #|add encodings later|#)
         :external-format *external-format*)))

(defmethod (setf destination-socket) (socket (obj connection))
  (setf (slot-value obj 'destination) socket
        (destination-stream obj) (socket-stream socket)))

(defmethod body-mode-p ((obj connection))
  (not (header-mode-p obj)))

(defmethod (setf previous-char) (char (obj connection))
  (psetf (slot-value obj 'previous-char) char
         (slot-value obj 'previous-char2) (slot-value obj 'previous-char)
         (slot-value obj 'previous-char3) (slot-value obj 'previous-char2)
         (slot-value obj 'previous-char4) (slot-value obj 'previous-char3)))

(defmethod headers-done-p ((obj connection))
  (and (char= (slot-value obj 'previous-char)
              (slot-value obj 'previous-char3)
              #\newline)
       (char= (slot-value obj 'previous-char2)
              (slot-value obj 'previous-char4)
              #\return)))

(defmethod add-thread-handler ((client usocket:usocket) (obj server))
  (bt:with-lock-held ((add-delete-lock obj))
    (push (make-instance 'client-connection :source-socket client)
          (server-connections obj))))

(defmacro with-lock-held-no-wait ((place) &body body)
  (let ((lock (gensym)))
    `(let ((,lock ,place))
       (when (bt:acquire-lock ,lock nil)
         (unwind-protect ,@body
           (bt:release-lock ,lock))))))

(defmethod header-mode ((obj connection))
  (setf (source-stream obj) (socket-stream (source-socket obj))
        (connection-buffer obj) (smart-buffer:make-smart-buffer)
        (buffer-stream obj) (make-instance 'smart-buffer-stream
                                           :buffer (connection-buffer obj))
        (previous-char obj) #\0
        (slot-value obj 'header-mode) t))

(defmethod read-headers ((obj connection))
  (with-open-stream
      (in (flex:make-flexi-stream
           (smart-buffer:finalize-buffer
            (connection-buffer obj))
           :external-format '(utf8 :eol-style :crlf)))
    (setf (headers obj)
          (typecase obj
            (client-connection (parse-request-headers in))
            (server-connection (parse-response-headers in))))))

(defmethod body-mode ((obj connection))
  (read-headers obj)
  (let ((new-buffer (smart-buffer:make-smart-buffer)))
    (setf (connection-buffer obj) new-buffer
          (buffer-stream obj) (make-instance 'smart-buffer-stream :buffer new-buffer)
          (previous-char obj) #\0
          (source-stream obj) (apply-encodings (source-stream obj) nil #|replace with actual encodings|#)
          (slot-value obj 'header-mode) nil)))

(defmethod body-mode :after ((obj client-connection))
  (unless (or (gethash :x-forwarded-for (headers obj))
              (gethash :forwarded (headers obj)))
    (let ((ip (str:join "." (coerce (usocket:get-peer-name (source-socket obj)) 'list))))
      (setf  (gethash :x-forwarded-for (headers obj)) ip
             (gethash :forwarded (headers obj)) (str:concat "for=" ip)))))

(defmethod body-done-p ((obj connection))
  (= (size (buffer-stream obj))
     (or (gethash :content-length (headers obj))
         0)))

(defmethod connect-server ((obj server) (client client-connection))
  (let* ((*headers* (headers client))
         (*request-headers* (request-headers client)))
    (multiple-value-bind
          (host port) (destination-parts (funcall (config-destinator (server-config obj))))
      (handler-case
          (let ((server-socket (socket-connect host port :element-type '(unsigned-byte 8))))
            (setf (destination-socket client) server-socket)
            (bt:with-lock-held ((add-delete-lock obj))
              (push (make-instance 'server-connection
                                   :source-socket server-socket
                                   :destination-socket (source-socket client)
                                   :request-headers (headers client))
                    (server-connections obj))))
        (connection-refused-error ()
          (respond-connection-refused client host port))))))

(defmethod write-headers ((obj client-connection) stream)
  (write-request-headers (headers obj) stream))

(defmethod write-headers ((obj server-connection) stream)
  (write-response-headers (headers obj) stream))

(defmethod processor ((obj client-connection) config)
  (config-request-processor config))

(defmethod processor ((obj server-connection) config)
  (config-response-processor config))

(defmethod send ((obj connection) config)
  (with-open-stream
      (input (flex:make-flexi-stream
              (smart-buffer:finalize-buffer (connection-buffer obj))
              :external-format *external-format*))
    (with-open-stream
        (input (if (null (processor obj config))
                   input
                   (let ((buffer (smart-buffer:make-smart-buffer)))
                     (with-open-stream
                         (output (make-instance 'smart-buffer-stream :buffer buffer))
                       (funcall (processor obj config)
                                input output)
                       (setf (header :content-length) (size output))
                       (flex:make-flexi-stream
                        (smart-buffer:finalize-buffer buffer)
                        :external-format *external-format*)))))
      (let ((destination-stream (destination-stream obj)))
        (write-headers obj destination-stream)
        (uiop:copy-stream-to-stream input destination-stream)
        (force-output destination-stream)))))

(defmethod send :around ((obj client-connection) config)
  (let* ((*headers* (headers obj))
         (*request-headers* (request-headers obj)))
    (call-next-method)))

(defmethod send :around ((obj server-connection) config)
  (let* ((*headers* (headers obj))
         (*request-headers* (request-headers obj))
         (*response-headers* (response-headers obj)))
    (call-next-method)))

(defmethod maybe-process-stage (server connection)
  (cond
    ((and (header-mode-p connection)
          (headers-done-p connection))
     (body-mode connection)
     t)
    ((and (body-mode-p connection)
          (body-done-p connection))
     (when (typep connection 'client-connection)
       (connect-server server connection))
     (send connection (server-config server))
     (typecase connection
       (server-connection
        (setf (garbagep connection) t))
       (client-connection
        (header-mode connection)))
     t)))

(defmethod work-connection ((obj server) (connection connection))
  (unless (garbagep connection)
    (handler-case
        (progn
          (loop with buffer = (buffer-stream connection)
                with input = (source-stream connection)
                for char = (read-char-no-hang input) do
                  (when char
                    (setf (previous-char connection) char)
                    (write-char char buffer))
                  (when (maybe-process-stage obj connection)
                    (return))
                while char)
          (maybe-process-stage obj connection))
      (stream-error (c)
        (format *error-output* "~A" c)
        (force-output *error-output*)
        (setf (garbagep connection) t)))))

(defmethod work ((obj server))
  (loop for connections = (server-connections obj) do
    (loop for connection in connections do
      (with-lock-held-no-wait ((connection-lock connection))
        (work-connection obj connection)))))

(defmethod make-worker ((obj server))
  (lambda () (work obj)))

(defun event-loop (port handler arguments)
  (bt:make-thread
   (lambda ()
     (handler-case
         (let* ((element-type '(unsigned-byte 8))
                (socket (usocket:socket-listen
                         usocket:*wildcard-host* port
                         :element-type element-type
                         :reuse-address t)))
           (unwind-protect
                (loop
                  (let ((client-socket (usocket:socket-accept
                                        socket :element-type element-type)))
                    (apply handler client-socket arguments)))
             (usocket:socket-close socket)))
       (address-in-use-error ()
         (format *error-output* "Port ~A is already being used by another program. Edit your configuration to use a different port or stop the other program.~%"
                 port))))
   :name "YXORP Server"))

(defmethod start ((obj server))
  (stop obj)
  (let ((config (server-config obj)))
    (prog1 obj
      (setf (slot-value obj 'adder)
            (event-loop (config-port config)
                        #'add-thread-handler (list obj))
            (slot-value obj 'workers)
            (loop repeat (config-workers config)
                  collect
                  (bt:make-thread (make-worker obj)
                                  :name "YXORP Worker"))))))

(defmethod start (config)
  (let ((config
          (cond ((stringp config) (read-config config))
                ((config-p config) config)
                (t (config)))))
    (start (make-instance 'server :config config))))

(defmethod clean-up ((obj connection))
  (close (source-stream obj) :abort t)
  (close (buffer-stream obj) :abort t)
  (when (slot-boundp obj 'destination-stream)
    (close (destination-stream obj) :abort t))
  (when (slot-boundp obj 'destination)
    (usocket:socket-close (destination-socket obj)))
  (usocket:socket-close (source-socket obj)))

(defmethod stop ((obj server))
  (flet ((destory (thread)
           (ignore-errors
            (bt:destroy-thread thread))))
    (destory (slot-value obj 'adder))
    (mapc #'destory (slot-value obj 'workers))
    (mapc #'clean-up (server-connections obj))
    (setf (server-connections obj) (list)))
  obj)

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
