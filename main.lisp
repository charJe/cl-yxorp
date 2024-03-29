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
  (handle-headers-and-body client server (config-request-filter config))
  (let* ((*headers* (alist->ht (parse-response-headers server)))
         (*response-headers* *headers*))
    (handle-headers-and-body server client (config-response-filter config))))

(defun filter-encodings (headers)
  (let ((*headers* headers))
    (setf (header :accept-encoding) nil)
    *headers*))

(defun save-ip (headers)
  (if (find "FORWARDED"
            headers
            :key (lambda (cons)
                   (symbol-name (car cons)))
            :test #'str:containsp)
      headers
      (let ((ip (str:join "." (map 'list #'princ-to-string usocket:*remote-host*))))
        (append
         (list (cons :x-forwarded-for ip))
         (list (cons :forwarded
                     (str:concat "for=" ip)))
         headers))))

(defun proxy-handler (client config)
  (track-thread (bt:current-thread))
  (with-socket-handler-case client
    (let* ((client (if (ssl-config-p (config-ssl config))
                       (make-ssl-stream client config)
                       client))
           (*headers*
             (-> client
               parse-request-headers
               save-ip
               alist->ht
               filter-encodings))
           (*request-headers* *headers*)
           (destination
             (make-destination (funcall (config-destinator config)))))
      (when destination
        (setf (header :host)
              (format nil "~A:~A" (host destination) (port destination)))
        (let ((host (host destination))
              (port (port destination)))
          (handler-case
              (let ((stream (socket-stream
                             (socket-connect
                              host port :element-type '(unsigned-byte 8)))))
                (with-open-stream
                    (server (if (destination-secure-p destination)
                                (cl+ssl:make-ssl-client-stream stream)
                                stream))
                (if (websocket-p)
                    (websocket-handler client server)
                    (http-handler client server config))))
            (usocket:connection-refused-error ()
              (format *error-output* "Could not connect to ~A.~%" destination))))))))

(defun ssl-redirect (client config)
  (with-socket-handler-case client
    (let* ((*headers* (alist->ht (parse-request-headers client)))
           (redirect-to (-> config config-ssl ssl-config-redirect-to)))
      (setq *headers*
            (alist->ht
             (list (cons :http-version "HTTP/1.1") (cons :status 301) (cons :message "Moved Permanently")
                   (cons :location
                         (str:concat
                          (format
                           nil "~A"
                           (make-instance
                            'destination
                            :protocol 'https
                            :host (first (str:split ":" (header :host) :omit-nulls t))
                            :port redirect-to))))
                   (header :uri))))
      (write-headers client))))

(defun start (config
              &aux (config
                    (cond ((stringp config) (read-config config))
                          ((config-p config) config)
                          (:else (config)))))
  (flet ((server (port handler name)
           (handler-case
               (track-thread
                (socket-server
                 usocket:*wildcard-host* port
                 handler (list config)
                 :element-type '(unsigned-byte 8)
                 :multi-threading t
                 :in-new-thread t
                 :name name))
             (address-in-use-error ()
               (format *error-output* "Port ~A is already being used by another program. Edit your configuration to use a different port or stop the other program.~%"
                       port)))))
    (some-> config
      config-ssl
      ssl-config-redirect-port
      (server 'ssl-redirect "YXORP SSL Redirect"))
    (server (config-port config) #'proxy-handler "YXORP Server")))

(defun stop ()
  (map-threads #'bt:destroy-thread))

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
