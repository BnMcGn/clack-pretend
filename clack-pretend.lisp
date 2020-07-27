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
  ;;Some middleware, such as :mount, will edit the env, causing later runs to fail
  (copy-list (gethash :input (elt *pretend-storage* index))))

(defun last-output (&optional (index 0))
  (gethash :output (elt *pretend-storage* index)))

(defun last-request-object (&optional (index 0))
  (lack.request:make-request (last-input index)))

(defun last-request-url ()
  (let ((req (last-input)))
    (concatenate
     'string
     (format nil "~a://" (string-downcase (princ-to-string
                                           (or (getf req :url-scheme)
                                               (getf req :uri-scheme)))))
     (getf req :server-name)
     (when-let ((port (getf req :server-port)))
       (unless (= 80 port)
         (format nil ":~d" port)))
     (getf req :request-uri))))

(defun last-session (&optional (index 0))
  (let ((input (last-input index)))
    (or (getf input :lack.session)
        (if (assoc :lack.session (getf input :cookies))
            (error "Session not found, but lack.session cookie is set. Try running pretend-builder with a higher :insert setting")
            (error "Session not found.")))))

(defun hash-table->source (ht)
  "Returns a source code representation of a hash table."
  `(hu:alist->hash ',(hu:hash->alist ht)
                :existing (make-hash-table :test #',(hash-table-test ht))))

(defun last-as-code (&optional (index 0))
  (let ((last (elt *pretend-storage* index)))
    `(hu:plist->hash
      (list
       :input
       ,(mapcar
         (lambda (x)
           (if (hash-table-p x)
               (hash-table->source x)
               x))
         (gethash :input last))
       :output ,(gethash :output last)))))

(defun quick-summary ()
  (mapcar
   (lambda (inp)
     (getf (gethash :input inp) :request-uri))
   *pretend-storage*))

;FIXME: should emit info about where listener will be placed.
(defmacro pretend-builder ((&key (insert 0) watch-symbols)
                           &rest middles-and-app)
  `(lack.builder:builder
    ,@(concatenate 'list
                   (subseq middles-and-app 0 insert)
                   `((lambda (app)
                       (pretend-component app ',watch-symbols)))
                   (subseq middles-and-app insert))))

(defun run-pretend (&key (index 0) path-info (app-chain *pretend-app-chain*))
  (declare (type integer index))
  (unless (< (1+ index) (length *pretend-storage*))
    (error "Session not found. Index too high or no sessions stored yet."))
  (unless (functionp app-chain)
    (error "Can't find a webapp to run."))
  (let ((env (last-input index)))
    (when path-info
      (push path-info env)
      (push :path-info env))
    (funcall app-chain env)))

(defun verbose-component (message)
  (lambda (app)
    (lambda (env)
      (print message)
      (funcall app env))))

(defmacro verbose-builder (&rest middles-and-app)
  (let ((accum nil))
    (dolist (itm middles-and-app)
      (push `(verbose-component "Component reached") accum)
      (push itm accum))
    `(lack.builder:builder
      ,@(nreverse accum))))

