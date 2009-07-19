;;;; Copyright 2009 Josh Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(5am:def-suite 5am:in-suite 5am:is))
  ;; God kills a kitten every time one of these gets written:
  (defmacro test (name &body body)
    `(5am:test ,name ,@body)))
