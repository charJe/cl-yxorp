(in-package #:yxorp)

(defvar *threads*-lock (bt:make-lock))
(defvar *threads* (list))

(defun track-thread (thread)
  (bt:with-lock-held (*threads*-lock)
    (push (tg:make-weak-pointer thread)
          *threads*)))

(defun map-threads (function)
  (declare (type function function))
  (bt:with-lock-held (*threads*-lock)
    (setq *threads* (delete-if-not 'tg:weak-pointer-value
                                   *threads*))
    (setq *threads* (delete-if-not
                     (lambda (pointer)
                       (-> pointer
                         tg:weak-pointer-value
                         bt:thread-alive-p))
                     *threads*))
    (map 'list
         (lambda (weak-pointer)
           (funcall function (tg:weak-pointer-value weak-pointer)))
         *threads*)))
