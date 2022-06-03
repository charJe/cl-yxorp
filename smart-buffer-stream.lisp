(in-package #:yxorp)

(defclass smart-buffer-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((smart-buffer :initarg :buffer
                 :initform (error "SMART-BUFFER-STREAM must have a buffer."))))

(defmethod trivial-gray-streams:stream-write-byte ((stream smart-buffer-stream) byte)
  (smart-buffer:write-to-buffer
   (slot-value stream 'smart-buffer)
   (make-array 1 :element-type '(unsigned-byte 8)
                 :initial-element byte
                 :adjustable nil
                 :fill-pointer nil
                 :displaced-to nil)))

(defmethod trivial-gray-streams:stream-write-char ((stream smart-buffer-stream) char)
  (smart-buffer:write-to-buffer
   (slot-value stream 'smart-buffer)
   (flex:string-to-octets (string char)
                          :external-format :utf8)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream smart-buffer-stream) sequence start end &key &allow-other-keys)
  (smart-buffer:write-to-buffer
   (slot-value stream 'smart-buffer)
   sequence
   (or start 0)
   (or end (length sequence))))

(defmethod trivial-gray-streams:stream-write-string ((stream smart-buffer-stream) string &optional start end)
  (let ((octets (flex:string-to-octets string
                                       :external-format :utf8)))
    (smart-buffer:write-to-buffer
     (slot-value stream 'smart-buffer)
     octets
     (or start 0)
     (if (eql end (length string))
         (length octets)
         (or end (length octets))))))
