;;;; clack-pretend.lisp

(in-package #:clack-pretend)

;;; "clack-pretend" goes here. Hacks and glory await!


(defparameter *pretend-storage-size* 10)
(defvar *pretend-storage* nil)

(defclass clack-pretend (lack.component:lack-component)
  ((watch-symbols :type list
                  :initarg :watch-symbols
                  :initform nil)))

(defmethod call ((this clack-pretend) env)
  (when (< *pretend-storage-size* (length *pretend-storage*))
    (setf *pretend-storage*
          (subseq *pretend-storage* 0 (1- *pretend-storage-size*))))
  (push (make-hash-table) *pretend-storage*)
  (setf (gethash :input (car *pretend-storage*)) env)
  (dolist (sym (slot-value this 'watch-symbols))
    (setf (gethash sym (car *pretend-storage*)) (symbol-value sym)))
  (let ((inner (call-next this env)))
    (setf (gethash :output (car *pretend-storage*)) inner)
    inner))

(defun last-input (&optional (index 0))
  (gethash :input (elt *pretend-storage* index)))

(defun last-request-object (&optional (index 0))
  (lack.request:make-request (last-input index)))

(defun last-request-url ()
  (let ((req (last-input)))
    (strcat
     (format nil "~a://" (string-downcase (mkstr (or (getf req :url-scheme)
                                                     (getf req :uri-scheme)))))
     (getf req :server-name)
     (awhen (getf req :server-port)
            (unless (= 80 it)
              (format nil ":~d" it)))
     (getf req :request-uri))))

;FIXME: should emit info about where listener will be placed.
(defvar *pretend-app-chain*)
(defmacro pretend-builder ((&key (insert 0) watch-symbols)
                           &rest middles-and-app)
  `(progn
     (setf *pretend-app-chain*
           (lack.builder:builder
            ,@(subseq middles-and-app insert)))
     (lack.builder:builder
      ,@(cat
         (subseq middles-and-app 0 insert)
         `((make-instance 'clack-pretend ,@(when watch-symbols
                                  (list :watch-symbols watch-symbols))))
         (subseq middles-and-app insert)))))

(defun run-pretend (&optional (index 0))
  (declare (type integer index))
  (unless (< (1+ index) (length *pretend-storage*))
    (error "Session not found. Index too high or no sessions stored yet."))
  (unless (functionp *pretend-app-chain*)
    (error "Can't find a webapp to run."))
  (funcall *pretend-app-chain*
           (gethash :input (elt *pretend-storage* index))))

