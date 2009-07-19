;;;; Copyright 2009 Josh Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Hacks go here. If appropriate, they should be sent to their project maintainers
;;;; once tested. This file is loaded before any other sykobot files, including utils.lisp
;;;; Please document why the hack is necessary, and make the librar(y|ies) it affects obvious.

;;; Good Medicine
;;; Please explain why this is necessary - syko
;;; I wish I knew. It's got to do with encoding hell. - Adlai
(setf drakma:*drakma-default-external-format* :utf-8
      flexi-streams:*substitution-char* #\?)

;;; Ralith's REPL hack for CL-IRC
;;; Stdout logging of raw IRC.
(defmethod cl-irc:irc-message-event :before (connection message)
  (declare (ignore connection))
  (format t "-> ~a~%"
          (let ((message (irc:raw-message-string message)))
            (subseq message 0 (1- (length message))))))

(defmethod cl-irc::send-irc-message :before (connection command &rest arguments)
  (declare (ignore connection))
  ;; make-irc-message includes a newline
  (format t "<- ~a"  (apply #'cl-irc::make-irc-message command arguments))
  (finish-output))

