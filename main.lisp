(In-package #:yxorp)

(defun websocket-handler (client server)
  (bt:make-thread
   (lambda ()
     (unwind-protect
          (forward-stream server client)
       (close client)))
   :name "YXORP Server->Client")
  (write-headers server)
  (forward-stream client server))

(defun http-handler (client server config)
  (let ((body (read-body client (config-request-filter config))))
    (write-body-and-headers body server))
  (let* ((*headers* (parse-response-headers server))
         (body (read-body server (config-response-filter config))))
    (write-body-and-headers body client)))

(defun proxy-handler (client config)
  (#+yxorp-build ignore-errors #-yxorp-build progn
   (when (string= "USOCKET Client" (bt:thread-name (bt:current-thread)))
     (setf (sb-thread:thread-name (bt:current-thread)) "YXORP Client->Server"))
   (let* ((client (if (ssl-config-p (config-ssl config))
                      (make-ssl-stream client config)
                      client))
          (*headers* (parse-request-headers client))
          (destination (funcall (config-destinator config))))
     (when (valid-destination-p destination)
       (multiple-value-bind (host port) (destination-parts destination)
         (with-open-stream
             (server (-> (socket-connect
                          host port :element-type '(unsigned-byte 8))
                       socket-stream))
           (if (websocket-p)
               (websocket-handler client server)
               (http-handler client server config))))))))

(defun ssl-redirect (client config)
  (#+yxorp-build ignore-errors #-yxorp-build progn
   (let* ((*headers* (parse-request-headers client))
          (destination (-> config config-ssl ssl-config-redirect-to)))
     (setq *headers*
           (list (cons :http-version "HTTP/1.1") (cons :status 301) (cons :message "Moved Permanently")
                 (cons :location
                       (str:concat
                        "https://" (first (str:split ":" (header :host) :omit-nulls t))
                        (when (/= 433 destination)
                          (format nil ":~a" destination))
                        (header :uri)))))
     (write-headers client))))

(defun start (config
              &aux (config
                    (cond ((stringp config) (read-config config))
                          ((config-p config) config)
                          (:else (config)))))
  (flet ((server (port handler name)
           (socket-server
            usocket:*wildcard-host* port
            handler (list config)
            :element-type '(unsigned-byte 8)
            :multi-threading t
            :in-new-thread t
            :name name)))
    (some-> config
      config-ssl
      ssl-config-redirect-port
      (server 'ssl-redirect "YXORP SSL Redirect"))
    (server (config-port config) 'proxy-handler "YXORP Server")))

(defun stop ()
  (->> (bt:all-threads)
    (remove-if-not
     (lambda (thread)
       (str:starts-with-p "YXORP" (bt:thread-name thread))))
    (mapc 'bt:destroy-thread)))

(defun main (&aux (args (uiop:command-line-arguments)))
  (start (nth 0 args))
  ;; this is done in favor of :in-new-thread nil to properly catch interrupt
  (handler-case
      (bt:join-thread
       (find "YXORP Server" (bt:all-threads)
             :key 'bt:thread-name
             :test 'string=))
    (#+sbcl sb-sys:interactive-interrupt
     #+clozure ccl:interrupt-signal-condition
     #+ecl interrupt-signal-condition
     () (uiop:quit))))
  
