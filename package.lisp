;;;; package.lisp

(defpackage #:clack-pretend
  (:use #:cl #:lack.component #:lack.request #:alexandria)
  (:export
   #:clack-pretend
   #:last-input
   #:last-request-object
   #:last-request-url
   #:pretend-builder
   #:run-pretend
   #:last-session
   #:last-output
   #:last-as-code
   #:quick-summary))

