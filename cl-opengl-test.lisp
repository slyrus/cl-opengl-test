
(in-package :cl-opengl-test)

(defvar *initial-thread-lock* (sb-thread:make-mutex :name "initial thread lock"))
(defvar *initial-thread-wait-lock* (sb-thread:make-mutex :name "initial thread wait lock"))
(defvar *initial-thread-queue* (sb-thread:make-waitqueue))
(defvar *initial-thread-data* nil)

(defun %invoke-on-initial-thread (fn wait)
  (sb-thread:interrupt-thread 
   (find "initial thread" (sb-thread:list-all-threads)
         :key 'sb-thread:thread-name
         :test 'equal)
   (lambda ()
     (funcall fn)
     (when wait
       (sb-thread:with-mutex (*initial-thread-wait-lock*)
         (push :done *initial-thread-data*)
         (sb-thread:condition-notify *initial-thread-queue*))))))

(defmacro on-initial-thread ((&key (wait t)) &body body)
  `(sb-thread:with-mutex (*initial-thread-lock*)
     (%invoke-on-initial-thread 
      (lambda ()
        ,@body)
      ,wait)
     ,@(when wait
             `((sb-thread:with-mutex (*initial-thread-wait-lock*)
                 (loop until *initial-thread-data*
                    do (sb-thread:condition-wait
                        *initial-thread-queue* *initial-thread-wait-lock*))
                 (pop *initial-thread-data*))))))
  

