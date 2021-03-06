;;;; Copyright 2009 Josh Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Unit tests for src/utils/time-utils.lisp
(in-package :sykobot)

(def-suite time-utils :in sykobot-tests)

(def-suite time-objects :in time-utils)
;;; TODO - make decoded-time-object and decoded-kilosecond-time tests check that zones
;;;        are handled properly.
(in-suite time-objects)

(test decoded-time-object
  (is (time-object-p (decoded-time-object (get-universal-time))))
  (let* ((universal-time (get-universal-time))
	 (time-object (decoded-time-object universal-time)))
    (multiple-value-bind (seconds minutes hours date month year day dstp zone)
	(decode-universal-time universal-time)
      (is (= seconds (time-seconds time-object)))
      (is (= minutes (time-minutes time-object)))
      (is (= hours (time-hours time-object)))
      (is (= date (time-date time-object)))
      (is (= month (time-month time-object)))
      (is (= year (time-year time-object)))
      (is (= day (time-day time-object)))
      (is (eq dstp (time-dstp time-object)))
      (is (= zone (time-zone time-object))))))

(test get-time-object
  (let ((time-object (get-time-object)))
    (multiple-value-bind (seconds minutes hours date month year day dstp zone)
	(get-decoded-time)
      ;; seconds are ignored because GET-TIME-OBJECT and GET-DECODED-TIME might not execute
      ;; within the same second, thus randomly causing the test to fail.
      ;; I'm willing to put quite a bit of faith in get-time-object if all other tests pass.
      (declare (ignore seconds)) 
      (is (= minutes (time-minutes time-object)))
      (is (= hours (time-hours time-object)))
      (is (= date (time-date time-object)))
      (is (= month (time-month time-object)))
      (is (= year (time-year time-object)))
      (is (= day (time-day time-object)))
      (is (eq dstp (time-dstp time-object)))
      (is (= zone (time-zone time-object))))))

(test decoded-kilosecond-time
  (let* ((universal-time (get-universal-time))
	 (ks-time (decoded-kilosecond-time universal-time)))
    (multiple-value-bind (seconds minutes hours)
	(decode-universal-time universal-time)
      (is (= (/ (+ seconds 
		   (* minutes 60)
		   (* hours 3600))
		1000.0)
	     ks-time)))))

(test get-kilosecond-time
  (let ((ks-time (get-kilosecond-time)))
    (multiple-value-bind (seconds minutes hours)
	(get-decoded-time)
      (is (= (/ (+ seconds 
		   (* minutes 60)
		   (* hours 3600))
		1000.0)
	     ks-time)))))
