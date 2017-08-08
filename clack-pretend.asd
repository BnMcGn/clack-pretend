;;;; clack-pretend.asd

(asdf:defsystem #:clack-pretend
  :description "Describe clack-pretend here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:clack
               #:alexandria
               #:gadgets
               #:lack-request
               #:cl-hash-util)
  :serial t
  :components ((:file "package")
               (:file "clack-pretend")))

