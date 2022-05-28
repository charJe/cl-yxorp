(in-package #:yxorp)

(defvar +crlf+
  (coerce (list (code-char 13) (code-char 10)) 'string))

(declaim (hash-table *request-headers*))
(defvar *request-headers*)
(declaim (hash-table *response-headers*))
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
  (declare (type string line))
  (let* ((pair (str:split ": " line :omit-nulls t))
         (name (make-keyword (first pair))))
    (cons name
          (if (member name *integer-headers*)
              (parse-integer (second pair))
              (second pair)))))

(defun %parse-request-headers (string)
  (let* ((lines (str:split +crlf+ string :omit-nulls t))
         (first-line (str:split " " (first lines))))
    (append
     (list (cons :method (first first-line))
           (cons :uri (second first-line))
           (cons :http-version (third first-line)))
     (mapcar #'parse-header-line (rest lines)))))

(defun parse-request-headers (stream)
  (%parse-request-headers (read-headers stream)))

(defun %parse-response-headers (string)
  (let* ((lines (str:split +crlf+ string :omit-nulls t))
         (first-line (str:split " " (first lines))))
    (append
     (list (cons :http-version (first first-line))
           (cons :status (parse-integer (second first-line)))
           (cons :message (str:join " " (cddr first-line))))
     (mapcar #'parse-header-line (rest lines)))))

(defun parse-response-headers (stream)
  (let ((response-header-string (read-headers stream)))
    (when (str:blankp response-header-string)
      (abort))
    (%parse-response-headers response-header-string)))

(defun read-headers (stream)
  (loop with end = (the simple-string (reverse (str:concat +crlf+ +crlf+)))
        with chars = (list)
        until (and (nth 4 chars)
                   (string= (coerce (subseq chars 0 4)
                                    'simple-string)
                            end))
        for byte = (read-byte stream nil)
        while byte
        for char = (code-char byte) do
          (push char chars)
        finally (return (coerce (reverse chars) 'string))))

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

(defun write-headers (stream)
  (-> *headers*
    ht->alist
    serialize-headers
    string-to-octets
    (write-sequence stream))
  (force-output stream))

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
