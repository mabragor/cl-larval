;;;; package.lisp

(defpackage #:cl-larval
  (:use #:cl #:iterate #:defmacro-enhance #:rutils.symbol)
  (:shadowing-import-from #:named-readtables #:parse-body)
  (:shadowing-import-from #:alexandria #:symbolicate)
  (:shadow #:! #:~ #:- #:* #:/ #:+ #:<< #:>> #:< #:> #:<= #:>= #:== #:!= #:& #:^ #:\| #:&& #:||
	   #:sleep #:set #:push #:pop
	   #:and #:or)
  (:export #:with-larval))


