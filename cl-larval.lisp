;;;; cl-larval.lisp
;;;; Alexander Popolitov, 2013, licenced under GPLv3
;;;; See COPYING for details on usage and copying.

(in-package #:cl-larval)

(cl-interpol:enable-interpol-syntax)

(defparameter *indent* 0 "Number of spaces to insert before the line.")
(defparameter *context* :code "Context of a program we are in. Can be :CODE, :DATA or :EEPROM")
(defparameter *stream* t)
(defparameter *comment* nil "Current comment to use")
(defparameter *label* nil "Current label to use")

(defun joinl (joinee lst)
  (format nil (concatenate 'string "~{~a~^" joinee "~}") lst))

(defun print-asm-line (directive operands &key (comment *comment*) (label *label*) (indent *indent*))
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

(define-asm-cmd (.equ equ define-symbol) (label expression)
  (print-asm-line ".equ" #?"$(label) = $(expression)"))

(define-asm-macro (.eseg eseg in-eeprom-segment) (&body body)
  "EEPROM segment"
  `(let ((*context* :eeprom))
     (print-asm-line ".eeprom" nil)
     ,@body))

(define-asm-cmd (.exit exit) ()
  (print-asm-line ".exit" nil))

(define-asm-cmd (.include .include) (filename)
  (print-asm-line ".include" `(,(format nil "~s" filename))))

(define-asm-cmd (.list listfile-generation-on) ()
  (print-asm-line ".list" nil))
(define-asm-cmd (.nolist nolist listfile-generation-off) ()
  (print-asm-line ".nolist" nil))

(defmacro with-no-listfile-generation (&body body)
  "Do not generate a listfile for a specified block of commands"
  `(progn (listfile-generation-off)
	  ,@body
	  (listfile-generation-on)))

(define-asm-cmd (.listmac listmac listfile-macro-on) ()
  (print-asm-line ".listmac" nil))

(define-asm-cmd (.org org set-origin) (expr &optional label)
  "Set program origin"
  (print-asm-line ".org" `(,expr) :label label))

(define-asm-cmd (.set set-symbol) (label expr)
  "Set a symbol equal to an expression. Label can be changed later in program."
  (print-asm-line ".set" #?"$(label) = $(expr)"))

;;; Built-in functions are here

(eval-always
  (defun %define-asm-builtin (symbol)
    (if (symbolp symbol)
	`(defun ,symbol (expr)
	   (concatenate 'string ,(string-downcase symbol) #?"($(expr))"))
	`(progn (eval-always ,(%define-asm-builtin (car symbol)))
		(abbr ,(cadr symbol) ,(car symbol))))))

(defmacro define-asm-builtins (&rest symbols)
  `(progn ,@(mapcar #'%define-asm-builtin symbols)))

(define-asm-builtins (low low-byte) (high high-byte) byte2 byte3 byte4
		     (lwrd low-word) (hwrd high-word) page exp2 log2)


;;; Built-in operands (Now I have absolutely no idea, how to track the parentheses in them.
;;; More precisely, how to omit unnecessary parentheses)

;;; Commands to the controller go here

(defmacro define-asm-instruction (names args &optional docstring)
  (let ((names (if (consp names) names (list names))))
    `(define-asm-cmd ,names ,args
       ,@(if docstring `(,docstring))
       (print-asm-line ,(string-downcase (car names))
		       (list ,@args)))))

(defmacro define-asm-instructions (&rest clauses)
  `(progn ,@(mapcar (lambda (x)
		      `(define-asm-instruction ,@x))
		    clauses)))

;;; Arithmetic and logic instructions
(define-asm-instructions
    ((add add-wo-carry) (dest-reg summand-reg) "Add without Carry")
    ((adc add-with-carry) (dest-reg summand-reg) "Add with Carry")
  ((adiw add-immediate-to-word) (dest-reg immediate) "Add Immediate to Word")
  ((sub subtract-wo-carry) (dest-reg subtractee) "Subtract SUBTRACTEE from DEST-REG without Carry.")
  ((subi subtract-immediate) (dest-reg immediate) "Subtract immediate")
  ((sbc subtract-with-carry) (dest-reg subtractee) "Subtract with Carry")
  ((sbci subtract-immediate-with-carry) (dest-reg immediate) "Subtract immediate with Carry")
  ((sbiw subtract-immediate-from-word) (dest-reg immediate) "Subtract immediate from word")
  ;; I must define AND and OR somehow separately
  ((andi and-with-immediate) (dest-reg immediate) "Logical AND with Immediate")
  ((ori or-with-immediate) (dest-reg immediate) "Logical OR with Immediate")
  ((eor exclusive-or) (dest-reg other-reg) "Exclusive OR")
  ((com ones-complement) (reg) "One's complement")
  ((neg twos-complement) (reg) "Two's complement")
  ((sbr set-bits) (reg immediate) "Set bit(s) in register")
  ((cbr clear-bits) (reg immediate) "Clear bit(s) in register")
  ((inc increment) (reg) "Increment")
  ((dec decrement) (reg) "Decrement")
  ((tst test-zerominus) (reg) "Test for zero or minus")
  ((clr clear-register) (reg) "Clear register")
  ((ser set-register) (reg) "Set register")
  ((mul unsigned-multiply) (one-reg other-reg) "R1, R0 <- Rd x Rr. Multiply unsigned"))

;;; Branch instructions
(define-asm-instructions
    ((rjmp relative-jump) (distance) "Relative jump")
    ((ijmp indirect-jump) () "Indirect jump to (Z)")
  ((jmp jump) (address) "Jump")
  ((rcall relative-call) (distance) "Relative call subroutine")
  ((icall indirect-call) () "Indirect call to (Z)")
  (call (address) "Call subroutine")
  ((ret subroutine-return) () "Subroutine return")
  ((reti interrupt-return) () "Interrupt return")
  ((cpse compare-skip-if-equal) (rd rr))
  ((cp compare) (rd rr))
  ((cpc compare-with-carry) (rd rr))
  ((cpi compare-with-immediate) (rd immediate))
  ((sbrc skip-if-not-bit) (reg bit) "Skip if bit in register cleared")
  ((sbrs skip-if-bit) (reg bit) "Skip if bit in register set")
  ((sbic skip-if-not-io-bit) (reg bit) "Skip if bit in I/O register cleared")
  ((sbis skip-if-io-bit) (reg bit) "Skip if bit in I/O register set")
  ((brbs branch-if-status) (s k) "Branch if status flag set")
  ((brbc branch-if-not-status) (s k) "Branch if status flag cleared")
  ((breq branch-if-equal) (distance) "If (Z = 1) then PC <- PC + DISTANCE + 1")
  ((brne branch-if-not-equal) (distance) "If (Z = 0) then PC <- PC + DISTANCE + 1")
  ((brcs branch-if-carry) (distance) "Branch if carry set")
  ((brcc branch-if-not-carry) (distance) "Branch if carry clear")
  ((brsh branch-if-same-higher) (distance) "Branch if same of higher")
  ((brlo branch-if-lower) (distance) "Branch if lower")
  ((brmi branch-if-minus) (distance) "Branch if minus")
  ((brpl branch-if-plus) (distance) "Branch if plus")
  ((brge branch-if-greater-equal) (distance) "Branch if greater or equal, signed")
  ((brlt branch-if-less) (distance) "Branch if less then, signed")
  ((brhs branch-if-half-carry) (distance) "Branch if half-carry set")
  ((brhc branch-if-not-half-carry) (distance) "Branch if half-carry clear")
  ((brts branch-if-t) (distance) "Branch if T flag set")
  ((brtc branch-if-not-t) (distance) "Branch if T flag clear")
  ((brvs branch-if-overflow) (distance) "Branch if overflow flag is set")
  ((brvc branch-if-not-overflow) (distance) "Branch if overflow flag is clear")
  ((brie branch-if-interrupts) (distance) "Branch if interrupts enabled")
  ((brid branch-if-not-interrupts) (distance) "Branch if interrupts disabled"))

;;; Data transfer instructions
(define-asm-instructions
    ((mov copy-register) (dest source) "Copy register")
    ((ldi load-immediate) (dest immediate) "Load immediate")
  ((lds load-direct) (dest addr) "Load direct from SRAM")
  ;; I need to specify, that REG-WITH-ADDR can be only X, Y or Z, and the postincrement and predecrements are possible
  ((ld load-indirect) (dest reg-with-addr) "Load indirect")
  ((ldd displacement-load-indirect) (dest reg-with-addr displacement)
   "Load indirect with displacement")
  ((sts store-direct) (addr src-reg) "Store direct into SRAM")
  ;; I need to specify, that REG-WITH-ADDR can be only X, Y or Z, and the postincrement and predecrements are possible
  ((st store-indirect) (reg-with-addr src-reg) "Store indirect")
  ((std displacement-store-indirect) (reg-with-addr displacement src-reg) "Store indirect with displacement")
  ((lpm load-program-memory) () "Load program memory")
  ((in port-in) (reg port) "In port")
  ((out port-out) (port reg) "Out port")
  ;; PUSH and POP need to be specified somehow
  )

;;; Bit and Bit-test instructions
(define-asm-instructions
    ((lsl shift-left) (reg) "Logical shift left")
    ((lsr shift-right) (reg) "Logical shift right")
  ((rol rotate-left) (reg) "Rotate left through carry")
  ((ror rotate-right) (reg) "Rotate right through carry")
  ((asr arithmetic-shift-right) (reg) "Arithmetic shift right")
  ((swap swap-nibbles) (reg))
  ((bset flag-set) (flag-name))
  ((bclr flag-clear) (flag-name))
  ((sbi set-io-bit) (port bit) "Set bit in I/O register")
  ((cbi clear-io-bit) (port bit) "Clear bit in I/O register")
  ((bst store-bit) (reg bit) "Bit store from register to T")
  ((bld load-bit) (reg bit) "Bit load from T to register")
  ((sec set-carry) () "Set carry")
  ((clc clear-carry) () "Clear carry")
  ((sen set-negative) () "Set negative flag")
  ((cln clear-negative) () "Clear negative flag")
  ((sez set-zero) () "Set zero flag")
  ((clz clear-zero) () "Clear zero flag")
  ((sei enable-interrupts) () "Global interrupt enable")
  ((cli disable-interrupts) () "Global interrupt disable")
  ((ses set-sign-test) () "Set sign test flag")
  ((cls clear-sign-test) () "Clear sign test flag")
  ((sev set-overflow) () "Set two's complement overflow")
  ((clv clear-overflow) () "Clear two's complement overflow")
  ;; I need a way to define a SET directive
  ((clt clear-t) () "Clear T in SREG")
  ((seh set-h) () "Set half-carry flag")
  ((clh clear-h) () "Clear half-carry flag")
  ((nop no-operation) () "No operation")
  ;; I need a way to define sleep
  ((wdr reset-watchdog) () "Watchdog reset")
  )
