(in-package #:yxorp)

(defvar +crlf+
  (coerce (list (code-char 13) (code-char 10)) 'string))

(declaim (type list *request-headers*))
(defvar *request-headers*)
(declaim (type list *response-headers*))
(defvar *response-headers*)
(declaim (type list *headers*))
(defvar *headers*)

(defvar *integer-headers*
  (list :content-length))

(defun make-keyword (name)
  (intern (str:upcase name)
          '#:keyword))

(defun read-sequence* (stream end)
  (let ((seq (make-array end)))
    (read-sequence seq stream :end end)
    seq))

(defun header (name &optional (headers *headers*))
  (declare (type (or keyword string) name))
  (if (stringp name)
      (header (make-keyword name) headers)
      (cdr (assoc name headers))))

(define-setf-expander header (name &optional (headers '*headers*) &environment env)
  (multiple-value-bind (vars vals new-value setter getter)
      (get-setf-expansion name env)
    (declare (ignore vars vals new-value setter))
    (let ((mplace (gensym))
          (mcons (gensym))
          (mname (gensym))
          (mnew-value (gensym)))
      (values
       (list mname mplace mcons)
       (list name headers `(assoc ,mname ,mplace))
       (list mnew-value)
       `(prog1 ,mnew-value
          (cond
            (,mcons
             (setf (cdr ,mcons) ,mnew-value))
            ((and ,mnew-value (consp ,mplace))
             (rplacd ,mplace
                     (cons
                      (cons ,mname ,mnew-value)
                      (cdr ,mplace))))
            (,mnew-value
             (setf ,headers (acons ,mname ,mnew-value nil)))
            (:else
             (setf ,headers (delete ,mname ,mplace :key 'car)))))
       `(header ,getter)))))

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
                            (header name headers)))
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
  (%parse-response-headers (read-headers stream)))

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

(defun read-body (stream filter)
  "Read an http body from STREAM and run it throught FILTER if the content-type
 header says it is utf-8."
  (declare (type (function (string) string) filter))
  (let* ((length (header :content-length))
         (utf8 (str:containsp "utf-8" (header :content-type) :ignore-case t))
         (body (when length (read-sequence* stream length))))
    (if (and utf8 length)
        (-<> body
          (octets-to-string :external-format :utf8)
          (funcall filter <>)
          (string-to-octets :external-format :utf8))
        body)))

(defun write-headers (stream)
  (write-sequence (string-to-octets (serialize-headers *headers*)) stream)
  (force-output stream))

(defun write-body-and-headers (body stream)
  (declare (type (or vector null) body))
  (when body
    (setf (header :content-length) (length body)))
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
