(in-package #:yxorp)

(defvar +crlf+
  (str:concat (list (code-char 13) (code-char 10))))

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


(defun rename-thread (thread new-name)
  (let ((name (bt:thread-name thread))
        (new-name-length (length new-name)))
    (loop for index from 0 below (length name) do
      (if (<= new-name-length index)
          (setf (elt name index) #\nul)
          (setf (elt name index)
            (elt new-name index))))))

(defun header (name &optional (headers *headers*))
  (if (stringp name)
      (header (make-keyword name) headers)
      (cdr (assoc name headers))))

(defun set-header (new-value name &optional (headers *headers*))
  (cond
    ((stringp name)
     (set-header new-value (make-keyword name)
                 headers))
    (new-value
     (let ((pair (find name headers :key 'car)))
       (if pair
           (setf (cdr pair) new-value)
           (push (cons name new-value)
                 headers))))
    (:else
     (delete name headers :key 'car))))

(defun (setf header) (new-value name &optional (headers *headers*))
  (set-header new-value name headers))

(defun serialize-headers (headers)
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
  (let* ((pair (str:split ": " line :omit-nulls t))
         (name (make-keyword (first pair))))
    (cons name
          (if (member name *integer-headers*)
              (parse-integer (second pair))
              (second pair)))))

(defun parse-request-headers (string)
  (let* ((lines (str:split +crlf+ string :omit-nulls t))
         (first-line (str:split " " (first lines))))
    (append
     (list (cons :method (first first-line))
           (cons :uri (second first-line))
           (cons :http-version (third first-line)))
     (map 'list 'parse-header-line (rest lines)))))

(defun parse-response-headers (string)
  (let* ((lines (str:split +crlf+ string :omit-nulls t))
         (first-line (str:split " " (first lines))))
    (append
     (list (cons :http-version (first first-line))
           (cons :status (parse-integer (second first-line)))
           (cons :message (str:join " " (cddr first-line))))
     (map 'list 'parse-header-line (rest lines)))))

(defun read-headers (stream)
  (loop with end = (reverse (str:concat +crlf+ +crlf+))
        with chars = (list)
        until (and (nth 4 chars)
                  (string= (str:concat (subseq chars 0 4))
                           end))
        for byte = (read-byte stream nil)
        while byte
        for char = (code-char byte) do
          (push char chars)
        finally (return (str:concat (reverse chars)))))

(defun forward-stream (origin destination)
  (loop for byte = (read-byte origin nil)
        while byte do
          (progn
            (write-byte byte destination)
            (force-output destination))))
