;;;; cl-larval.asd

(asdf:defsystem #:cl-larval
  :serial t
  :description "Lisp syntax for assembler for AVR microcontrollers"
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:iterate #:defmacro-enhance #:cl-interpol #:rutils #:cl-curlex)
  :components ((:file "package")
               (:file "cl-larval")))

