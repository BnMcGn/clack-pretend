;;;; clack-pretend.asd

(asdf:defsystem #:clack-pretend
  :description "A testing and debugging tool for Clack"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:clack
               #:alexandria
               #:lack-request
               #:cl-hash-util)
  :serial t
  :components ((:file "package")
               (:file "clack-pretend")))

