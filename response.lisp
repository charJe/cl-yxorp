(in-package yxorp)

(defmethod respond-connection-refused ((obj connection) host port)
  (format *error-output* "connection refused~%"))

(defmethod respond-connection-timeout ((obj connection))
  (format *error-output* "connection timeout~%"))
