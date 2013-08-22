;;;; cl-larval.lisp
;;;; Alexander Popolitov, 2013, licenced under GPLv3
;;;; See COPYING for details on usage and copying.

(in-package #:cl-larval)

(cl-interpol:enable-interpol-syntax)

(defparameter *indent* 0 "Number of spaces to insert before the line.")
(defparameter *context* :code "Context of a program we are in. Can be :CODE, :DATA or :EEPROM")
(defparameter *stream* t)
(defparameter *comment* nil "Current comment to use")

(defun joinl (joinee lst)
  (format nil (concatenate 'string "~{~a~^" joinee "~}") lst))

(defun print-asm-line (directive operands &key (comment *comment*) label (indent *indent*))
  "Most general function to output ASM code-line."
  (with-standard-io-syntax
    (format *stream* "~a~a~%"
	    (make-string indent :initial-element #\space)
	    (joinl " " (remove-if-not #'identity (list (if label #?"$(label):")
						       directive
						       (if operands (joinl ", " operands))
						       (if comment #?"; $(comment)")))))))

(defun comment (comment)
  (print-asm-line nil nil :comment comment))
(defun empty-line ()
  (print-asm-line nil nil))

(defmacro %define-asm-cmd (type names args &body body)
  "Conveniently define several names for a command."
  (let ((names (if (atom names) (list names) names)))
    `(progn (eval-always
	      (,(case type
		      (:function 'defun)
		      (:macro 'defmacro))
		,(car names) ,args
		,@body)
	      ,@(mapcar (lambda (x)
			  `(abbr ,x ,(car names)))
			(cdr names))))))

(defmacro define-asm-cmd (names args &body body)
  `(%define-asm-cmd :function ,names ,args ,@body))
(defmacro define-asm-macro (names args &body body)
  `(%define-asm-cmd :macro ,names ,args ,@body))
;;; Directives to the assembler go here

(define-asm-cmd (.byte reserve-byte) (label expr)
  "Reserve bytes to a variable"
  (if (not (eq *context* :data))
      (error "BYTE directive can only be used in data segment, see DSEG")
      (print-asm-line ".byte" `(,expr) :label label)))

(define-asm-macro (.cseg cseg in-code-segment) (&body body)
  "Code segment"
  `(let ((*context* :code))
     (print-asm-line ".cseg" nil)
     ,@body))
    
(define-asm-cmd (.db db define-byte) (label &rest exprs)
  "Define constant byte(s) in program memory of EEPROM memory"
  (if (not (or (eq *context* :eeprom)
	       (eq *context* :code)))
      (error ".DB directive can only appear in CODE or EEPROM contexts")
      (if (not exprs)
	  (error ".DB directive must have at least one parameter")
	  (print-asm-line ".db" exprs :label label))))

(define-asm-cmd  (.def def alias-register) (symbol register)
  "Define a symbolic name for a register. Register can be a symbol, string or a number.
In latter case 'R' is prepended automatically"
  (let ((register (if (numberp register)
		      #?"r$(register)"
		      register)))
    (print-asm-line ".def" `(,#?"$(symbol)=$(register)"))))

(define-asm-cmd (.device device) (name)
    "Specify an AVR device to compile for."
    (print-asm-line ".device" `(,name)))

(define-asm-macro (.dseg dseg in-data-segment) (&body body)
  "Data segment"
  `(let ((*context* :data))
     (print-asm-line ".dseg" nil)
     ,@body))

(define-asm-cmd (.dw dw define-word) (label &rest exprs)
  "Define constant word(s) in program memory of EEPROM memory"
  (if (not (or (eq *context* :eeprom)
	       (eq *context* :code)))
      (error ".DW directive can only appear in CODE or EEPROM contexts")
      (if (not exprs)
	  (error ".DW directive must have at least one parameter")
	  (print-asm-line ".dw" exprs :label label))))

(define-asm-cmd endmacro ()
  ".ENDMACRO directive"
  (print-asm-line ".endmacro" nil))

(define-asm-cmd startmacro (name)
  (print-asm-line ".macro" `(,name)))

(define-asm-macro (defmacro-asm define-assembler-macro) (name args &body body)
  (multiple-value-bind (forms decls doc) (sb-int::parse-body body)
    (cond (decls (error "Something wrong: declarations should not appear in definition of assembler macro"))
	  ((> (length args) 10) (error "Only ten arguments to assembler macro required"))
	  (t `(progn ,(if doc
			  `(let ((*comment* ,doc))
			     (startmacro ,(if (symbolp name) `(quote ,name))))
			  `(startmacro ,(if (symbolp name) `(quote ,name))))
		     (let ,(iter (for arg in args)
				 (for i from 0)
				 (collect `(,arg ,#?"@$(i)")))
		       (declare (ignorable ,@args))
		       (let ((*indent* (+ *indent* 2)))
			 ,@forms))
		     (endmacro))))))
    
;;; Commands to the controller go here

