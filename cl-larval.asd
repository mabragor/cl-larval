;;;; cl-larval.asd

(asdf:defsystem #:cl-larval
  :serial t
  :description "Lisp syntax for assembler for AVR microcontrollers"
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:iterate #:cl-interpol #:rutils #:cl-curlex #:named-readtables
			 #:alexandria #:cl-package-locks)
  :components ((:file "package")
               (:file "cl-larval")))

