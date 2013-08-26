;;;; package.lisp

(defpackage #:cl-larval
  (:use #:cl #:iterate #:defmacro-enhance #:rutils.symbol)
  (:shadow #:! #:~ #:- #:* #:/ #:+ #:<< #:>> #:< #:> #:<= #:>= #:== #:!= #:& #:^ #:\| #:&& #:||
	   #:sleep #:set #:push #:pop
	   #:and #:or)
  (:export #:with-larval))


