(In-package #:yxorp)

(defun proxy-handler (client config)
  (#+yxorp-build ignore-errors #-yxorp-build progn
   (when (string= "USOCKET Client" (bt:thread-name (bt:current-thread)))
     (rename-thread (bt:current-thread) "YXORP Client->Server"))
   (let* ((client
            (let ((ssl (config-ssl config)))
              (if ssl
                  (cl+ssl:make-ssl-server-stream
                   client
                   :certificate (ssl-config-certificate ssl)
                   :key (ssl-config-key ssl)
                   :password (ssl-config-password ssl))
                  client)))
          (*headers* (parse-request-headers client))
          (destination (funcall (config-destinator config))))
     (when destination
       (multiple-value-bind
             (host port)
           (typecase destination
             (integer
              (values "localhost" destination))
             (string
              (values-list (str:split ":" destination :omit-nulls t))))
         (with-open-stream
             (server
              (socket-stream
               (socket-connect
                host port :element-type '(unsigned-byte 8))))
           (if (string= "websocket" (str:downcase (header :upgrade)))
               ;; if websocket
               (progn
                 (bt:make-thread
                  (lambda ()
                    (unwind-protect
                         (forward-stream server client)
                      (close client)))
                  :name "YXORP Server->Client")
                 (write-sequence (map 'list 'char-code (serialize-headers *headers*)) server)
                 (force-output server)
                 (forward-stream client server))
               ;; else: regular http request
               ;; therefore, allow the user defined handlers
               (progn
                 (let* ((length (header :content-length))
                        (utf8 (str:containsp "utf-8" (header :content-type)))
                        (body (when length (read-sequence* server length)))
                        (body (if (and utf8 length)
                                  (as-> body it
                                    (flex:octets-to-string it :external-format :utf8)
                                    (funcall (config-request-filter config) it)
                                    (flex:string-to-octets it :external-format :utf8))
                                  body)))
                   (when body
                     (setf (header :content-length) (length body)))
                   (write-sequence (map 'list 'char-code (serialize-headers *headers*)) server)
                   (write-sequence body server)
                   (force-output server))
                        (length (header :content-length))
                        (utf8 (str:containsp "utf-8" (header :content-type)))
                        (body (when length (read-sequence* server length)))
                        (body (if (and utf8 length)
                                  (as-> body it
                                    (flex:octets-to-string it :external-format :utf8)
                                    (funcall (config-response-filter config) it)
                                    (flex:string-to-octets it :external-format :utf8))
                                  body)))
                   (when body
                     (setf (header :content-length) (length body)))
                   (write-sequence (map 'list 'char-code (serialize-headers *headers*)) client)
                   (write-sequence body client)
                   (force-output client))))))))))

(defun ssl-redirect (client config)
  (#+yxorp-build ignore-errors #-yxorp-build progn
  (let* ((client (flex:make-flexi-stream client :external-format :utf8))
         (destination (ssl-config-redirect-destination (config-ssl config))))
    (-> (list (cons :http-version "HTTP/1.1") (cons :status 301) (cons :message "Moved Permanently")
              (cons :location
                    (str:concat
                     "https://" (first (str:split ":" (header :host) :omit-nulls t))
                     (when (/= 433 destination)
                       (format nil ":~a" destination))
                     (header :uri))))
      serialize-headers
      (write-sequence client))
    (force-output client))))
   (let* ((*headers* (parse-request-headers client))

(defun start (config
              &aux (config (or (and (stringp config)
                                    (read-config config))
                               config
                               (config))))
  (some-<> config
    config-ssl
    ssl-config-redirect-port
    (socket-server usocket:*wildcard-host* <>
                   'ssl-redirect (list config)
                   :element-type '(unsigned-byte 8)
                   :multi-threading t
                   :in-new-thread t
                   :name "YXORP SSL Redirect"))
  (socket-server usocket:*wildcard-host*
                 (config-port config)
                 'proxy-handler (list config)
                 :element-type '(unsigned-byte 8)
                 :multi-threading t
                 :in-new-thread t
                 :name "YXORP Server"))

(defun stop ()
  (->> (bt:all-threads)
    (remove-if-not
     (lambda (thread)
       (str:starts-with-p "YXORP" (bt:thread-name thread))))
    (mapc 'bt:destroy-thread)))

(defun main (&aux (args #+sbcl sb-ext:*posix-argv*))
  (start (nth 1 args))
  ;; this is done in favor of :in-new-thread nil to properly catch interrupt
  (handler-case
      (bt:join-thread
       (find "YXORP Server" (bt:all-threads)
             :key 'bt:thread-name
             :test 'string=))
    (#+sbcl sb-sys:interactive-interrupt
     () (uiop:quit))))
  
