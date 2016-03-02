;;;; clack-pretend.asd

(asdf:defsystem #:clack-pretend
  :description "Describe clack-pretend here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:clack
               #:alexandria
               #:anaphora
               #:gadgets
               #:lack-request)
  :serial t
  :components ((:file "package")
               (:file "clack-pretend")))

