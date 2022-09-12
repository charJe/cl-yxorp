(in-package #:yxorp)

(setq chunga:*accept-bogus-eols* t)

(deftype encoding ()
  '(member :chunked :gzip :deflate))

(defun encodingp (thing)
  (typep thing 'encoding))

(defun chunkify (stream)
  (let ((chunked (chunga:make-chunked-stream stream)))
    (setf (chunga:chunked-stream-input-chunking-p chunked) t)
    chunked))

(defun apply-decoding (stream encoding)
  (case encoding
    (:chunked (chunkify stream))
    (:gzip (chipz:make-decompressing-stream :gzip stream))
    (:deflate (chipz:make-decompressing-stream :deflate stream))
    (otherwise stream)))

(defun apply-decodings (stream decodings)
  (reduce 'apply-decoding decodings :initial-value stream))

(defun apply-encoding (stream encoding)
  (case encoding
    (:chunked (chunkify stream))
    (:gzip (salza2:make-compressing-stream :gzip stream))
    (:deflate (salza2:make-compressing-stream :deflate stream))
    (otherwise stream)))

(defun apply-encodings (stream encodings)
  (reduce 'apply-encoding encodings :initial-value stream))
