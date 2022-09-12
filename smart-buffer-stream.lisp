(in-package #:yxorp)

(defclass smart-buffer-stream (trivial-gray-streams:fundamental-output-stream)
  ((smart-buffer :initarg :buffer
                 :initform (error "SMART-BUFFER-STREAM must have a buffer."))
   (size :initform 0 :accessor size)))

(defmethod trivial-gray-streams:stream-write-byte ((stream smart-buffer-stream) byte)
  (incf (size stream))
  (smart-buffer:write-to-buffer
   (slot-value stream 'smart-buffer)
   (make-array 1 :element-type '(unsigned-byte 8)
                 :initial-element byte
                 :adjustable nil
                 :fill-pointer nil
                 :displaced-to nil)))

(defmethod trivial-gray-streams:stream-write-char ((stream smart-buffer-stream) char)
  (let ((octets (flex:string-to-octets (string char) :external-format :utf8)))
    (incf (size stream) (length octets))
    (smart-buffer:write-to-buffer
     (slot-value stream 'smart-buffer)
     octets)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream smart-buffer-stream) sequence start end &key &allow-other-keys)
  (let ((start (or start 0))
        (end (or end (length sequence))))
    (incf (size stream) (- end start))
    (smart-buffer:write-to-buffer
     (slot-value stream 'smart-buffer)
     sequence start end)))

(defmethod trivial-gray-streams:stream-write-string ((stream smart-buffer-stream) string &optional (start 0) end)
  (let ((octets (flex:string-to-octets (subseq string start end) :external-format :utf8)))
    (incf (size stream) (length octets))
    (smart-buffer:write-to-buffer
     (slot-value stream 'smart-buffer)
     octets)))
