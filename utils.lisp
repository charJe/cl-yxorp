(in-package #:yxorp)

(declaim (hash-table *request-headers*))
(defvar *request-headers*)
(declaim ((or null hash-table) *response-headers*))
(defvar *response-headers*)
(declaim (hash-table *headers*))
(defvar *headers*)

(defvar *integer-headers*
  (list :content-length))

(defun make-keyword (name)
  (intern (str:upcase name)
          '#:keyword))

(defun read-sequence* (stream)
  (let ((content-length (header :content-length)))
    (cond
      ((or (not (or content-length
                    (header :transfer-encoding)))
           (member (header :method) (list "get" "head") :test 'equalp))
       #())
      ((header :content-length)
       (let ((seq (make-array content-length :element-type '(unsigned-byte 8))))
         (read-sequence seq stream :end content-length)
         seq))
      (:else
       (loop with result = (list)
             for byte = (handler-case (read-byte stream nil nil)
                          (end-of-file nil))
             while byte do
               (push byte result)
             finally (return (coerce (reverse result) 'vector)))))))

(defun header (name &optional (headers *headers*))
  (declare (keyword name) (hash-table headers))
  (gethash name headers))

(defun (setf header) (new-value name &aux (headers *headers*))
  (declare (keyword name) (hash-table headers))
  (if new-value
      (setf (gethash name headers) new-value)
      (remhash name headers)))

(defun serialize-headers (headers)
  (declare (type list headers))
  (let ((top (list :method :uri :http-version :status :message)))
  (str:join
   +crlf+
   (append (list
            (str:join
             " " (->> top
                   (mapcar
                    (lambda (name)
                      (cdr (assoc name headers))))
                   (remove nil)
                   (mapcar #'princ-to-string))))
           (as-> headers it
             (remove-if (lambda (name)
                          (declare (type keyword name))
                          (member name top))
                        it :key 'car)
             (mapcar
              (lambda (pair)
                (str:join
                 ": " (list
                       (-> pair car symbol-name str:downcase)
                       (princ-to-string (cdr pair)))))
              it))
           (list +crlf+)))))
  
(defun parse-header-line (line)
  (declare (string line))
  (let* ((pair (str:split ": " line :omit-nulls t))
         (name (make-keyword (first pair))))
    (values name
            (if (member name *integer-headers*)
                (parse-integer (second pair))
                (second pair)))))

(defun parse-headers (specials stream)
  (loop with headers = (make-hash-table)
        for line = (read-line stream)
        for first-line = t then nil
        while (< 0 (length line)) do
          (if first-line
              (let ((parts (str:split " " line)))
                (mapc (lambda (name value)
                        (setf (gethash name headers) value))
                      specials parts))
              (multiple-value-bind
                    (name value) (parse-header-line line)
                (setf (gethash name headers) value)))
        finally (return headers)))

(defparameter *request-specials*
  '(:method :uri :http-version))

(defparameter *response-specials*
  '(:http-version :status :message))

(defun parse-request-headers (stream)
  (parse-headers *request-specials* stream))
                 
(defun parse-response-headers (stream)
  (parse-headers *response-specials* stream))

(defun %write-headers (specials headers stream)
  (write-line (str:join
               " " (mapcar (lambda (key)
                             (gethash key headers))
                           specials))
              stream)
  (maphash
   (lambda (key value)
     (unless (member key specials)
       (format stream "~A: ~A~%" key value)))
   headers)
  (terpri stream))

(defun write-request-headers (headers stream)
  (%write-headers *request-specials* headers stream))

(defun write-response-headers (headers stream)
  (%write-headers *response-specials* headers stream))

(defun forward-stream (origin destination)
  (loop for byte = (ignore-errors
                    (read-byte origin nil))
        while byte do
          (ignore-errors
            (write-byte byte destination)
            (force-output destination))))

(defun websocket-p ()
  (string= "websocket"
           (let ((upgrade (str:downcase (header :upgrade))))
             (the simple-string
                  (if upgrade
                      upgrade
                      "")))))

(defun extract-encodings-from (header)
  (declare (keyword header))
  (some-<>> header
    header
    (str:split ", " <> :omit-nulls t)
    (mapcar #'str:trim)
    (mapcar #'str:upcase)
    (mapcar #'make-keyword)
    (remove-if-not #'encodingp)))

(defun extract-charset ()
  (or (some-<>> :content-type
        header
        (str:split ";" <> :omit-nulls t)
        (mapcar #'str:trim)
        (find "charset" <> :test #'str:starts-with-p)
        (str:split "=" <> :omit-nulls t)
        second
        str:upcase
        make-keyword)
      (when (str:containsp "text" (header :content-type))
        :iso-8859-1)))

(defun how-much-to-read ()
  (or (header :content-length)
      
      0))

(defun forward-body (filter in out)
  "Read an http body from IN stream and run it throught FILTER.
FILTER will then send output to the OUT stream."
  (let* ((transfer-encodings (extract-encodings-from :transfer-encoding))
         (content-encodings (extract-encodings-from :content-encoding))
         (input-stream
           (-> in
             (apply-decodings (reverse transfer-encodings))
             (apply-decodings (reverse content-encodings))))
         (output-stream
           (->> content-encodings
             (cons :chunked)
             remove-duplicates
             (apply-encodings out))))
    (setf (header :transfer-encoding) nil
          (header :content-encoding) content-encodings)
    (funcall filter input-stream output-stream)))

(defun write-body-and-headers (body stream)
  (declare (type (or vector null) body))
  (write-headers stream)
  (write-sequence body stream)
  (force-output stream))

(defun make-ssl-stream (stream config)
  (let ((ssl-config (config-ssl config)))
    (cl+ssl:make-ssl-server-stream
     stream
     :certificate (ssl-config-certificate ssl-config)
     :key (ssl-config-key ssl-config)
     :password (ssl-config-password ssl-config))))
