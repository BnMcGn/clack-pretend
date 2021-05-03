;;;; clack-pretend.lisp

(in-package #:clack-pretend)

;;; "clack-pretend" goes here. Hacks and glory await!


(defparameter *pretend-storage-size* 10)
(defvar *pretend-storage* nil)
(defvar *logfile* nil)
(defvar *watch-symbols* nil)

(defvar *pretend-app-chain*)

(defun store-results (input output)
  (when (< *pretend-storage-size* (length *pretend-storage*))
    (setf *pretend-storage*
          (subseq *pretend-storage* 0 (1- *pretend-storage-size*))))
  (push (make-hash-table) *pretend-storage*)
  (dolist (sym *watch-symbols*)
    (setf (gethash sym (car *pretend-storage*)) (symbol-value sym)))
  (setf (gethash :input (car *pretend-storage*)) input)
  (setf (gethash :output (car *pretend-storage*)) output)
  (when *logfile*
    (with-open-file (s *logfile* :direction :output :if-exists :append :if-does-not-exist :create)
      (terpri s)
      (princ "Clack-pretend request dump:" s)
      (terpri s)
      (princ (last-as-code) s))))

(defun pretend-component (app watch-symbols error-only logfile)
  (lambda (env)
    ;; Need this to rerun POST requests. Normally this is done in lack.request
    (unless (typep (getf env :raw-body) 'circular-streams:circular-input-stream)
      (setf (getf env :raw-body) (circular-streams:make-circular-input-stream (getf env :raw-body))))
    (let* ((*watch-symbols* watch-symbols)
           (*logfile* logfile)
           (input (copy-list env))
           (output (handler-case (funcall app env)
                     (error (c)
                       (store-results input c)
                       (error c)))))
      (if error-only
          (if (and (listp output) (integerp (car output)) (< 499 (car output) 600))
              (progn (store-results input output)
                     output)
              output)
          (progn (store-results input output)
                 output)))))

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
(defmacro pretend-builder ((&key (insert 0) watch-symbols errors-only logfile)
                           &rest middles-and-app)
  `(lack.builder:builder
    ,@(concatenate 'list
                   (subseq middles-and-app 0 insert)
                   `((lambda (app)
                       (pretend-component app ',watch-symbols ,errors-only ,logfile)))
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

