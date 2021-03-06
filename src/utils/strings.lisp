;;;; Copyright 2009 Josh Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

(defmacro define-str-util (util-name orig-name)
  `(defun ,util-name (str seq &rest keys &key
                      (test #'string-equal) &allow-other-keys)
     (apply #',orig-name str seq :test test keys)))

(define-str-util str-find find)
(define-str-util str-member member)
(define-str-util str-assoc assoc)
(define-str-util str-remove remove)
(define-str-util str-delete delete)
(define-str-util str-count count)
(define-str-util str-position position)

(defun str-substitute (new old seq &rest keys &key
                       (test #'string-equal) &allow-other-keys)
  (apply #'substitute new old seq :test test keys))

(defmacro str-pushnew (str seq &rest keys &key
                       (test #'string-equal) &allow-other-keys)
  `(pushnew ,str ,seq :test ,test ,@keys))
