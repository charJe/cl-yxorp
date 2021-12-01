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
                   (map 'list
                        (lambda (name)
                          (cdr (assoc name headers))))
                   (remove nil)
                   (map 'list 'princ-to-string))))
           (as-> headers it
             (remove-if (lambda (name)
                          (declare (type keyword name))
                          (member name top))
                        it :key 'car)
             (map 'list
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
     (map 'list 'parse-header-line (rest lines)))))

(defun parse-request-headers (stream)
  (%parse-request-headers (read-headers stream)))

(defun %parse-response-headers (string)
  (let* ((lines (str:split +crlf+ string :omit-nulls t))
         (first-line (str:split " " (first lines))))
    (append
     (list (cons :http-version (first first-line))
           (cons :status (parse-integer (second first-line)))
           (cons :message (str:join " " (cddr first-line))))
     (map 'list 'parse-header-line (rest lines)))))

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
  (some->> header
    header
    (str:split ", ")
    (map 'list 'str:trim)
    (map 'list 'str:upcase)
    (map 'list 'make-keyword)
    (remove-if-not 'encodingp)))

(defun extract-charset ()
  (or (some-<>> :content-type
        header
        (str:split ";")
        (map 'list 'str:trim)
        (find "charset" <> :test 'str:starts-with-p)
        (str:split "=")
        second
        str:upcase
        make-keyword)
      (when (str:containsp "text" (header :content-type))
        :iso-8859-1)))

(defun read-body (stream filter)
  "Read an http body from STREAM and run it throught FILTER."
  (let* ((str:*omit-nulls* t)
         (charset (extract-charset))
         (transfer-encodings (extract-encodings-from :transfer-encoding))
         (content-encodings (extract-encodings-from :content-encoding))
         (body (-> stream
                 (apply-decodings (reverse transfer-encodings))
                 (apply-decodings (reverse content-encodings))
                 read-sequence*)))
    (setf (header :transfer-encoding) nil
          (header :content-encoding) content-encodings)
    (let ((output-body
            (handler-bind
                ((flexi-streams:external-format-encoding-error
                   (lambda (condition) (declare (ignore condition))
                     (invoke-restart (find-restart 'use-value)
                                     #\replacement_character))))
              (-<> body
                (octets-to-string :external-format (or charset :iso-8859-1))
                (funcall filter <>)
                (string-to-octets :external-format (or charset :iso-8859-1))))))
      (let ((length (length output-body)))
          (setf (header :content-length)
                (unless (= 0 length)
                  length)))
      (-> output-body
        flex:make-in-memory-input-stream
        (apply-encodings content-encodings)
        read-sequence*))))

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
