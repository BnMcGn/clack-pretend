;;;; clack-pretend.lisp

(in-package #:clack-pretend)

;;; "clack-pretend" goes here. Hacks and glory await!


(defparameter *pretend-storage-size* 10)
(defvar *pretend-storage* nil)

(defvar *pretend-app-chain*)

(defun pretend-component (app watch-symbols)
  (setf *pretend-app-chain* app)
  (lambda (env)
    (when (< *pretend-storage-size* (length *pretend-storage*))
      (setf *pretend-storage*
            (subseq *pretend-storage* 0 (1- *pretend-storage-size*))))
    (push (make-hash-table) *pretend-storage*)
    (setf (gethash :input (car *pretend-storage*)) (copy-list env))
    (dolist (sym watch-symbols)
      (setf (gethash sym (car *pretend-storage*)) (symbol-value sym)))
    (let ((inner (funcall app env)))
      (setf (gethash :output (car *pretend-storage*)) inner)
      inner)))

(defun last-input (&optional (index 0))
  (gethash :input (elt *pretend-storage* index)))

(defun last-output (&optional (index 0))
  (gethash :output (elt *pretend-storage* index)))

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

(defun last-session (&optional (index 0))
  (let ((input (last-input index)))
    (anaphora:aif (getf input :lack.session)
                  anaphora:it
                  (if (assoc :lack.session (getf input :cookies))
                      (error "Session not found, but lack.session cookie is set. Try running pretend-builder with a higher :insert setting")
                      (error "Session not found.")))))

;FIXME: should emit info about where listener will be placed.
(defmacro pretend-builder ((&key (insert 0) watch-symbols)
                           &rest middles-and-app)
  `(lack.builder:builder
    ,@(concatenate 'list
                   (subseq middles-and-app 0 insert)
                   `((lambda (app)
                       (pretend-component app ',watch-symbols)))
                   (subseq middles-and-app insert))))

(defun run-pretend (&optional (index 0))
  (declare (type integer index))
  (unless (< (1+ index) (length *pretend-storage*))
    (error "Session not found. Index too high or no sessions stored yet."))
  (unless (functionp *pretend-app-chain*)
    (error "Can't find a webapp to run."))
  (funcall *pretend-app-chain*
           (gethash :input (elt *pretend-storage* index))))

