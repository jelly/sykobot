;;;; Copyright 2009 Josh Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

;;; Modularization of commands
(defproto command-bot ((proto 'listener-bot))
  ((commands (make-hash-table :test #'equal))
   (detection-regex nil)
   (command-prefix "@")))

(defproto command ()
  ((cmd-function (constantly "OOPS. Don't go here."))
   (dox "No documentation available.")))

;;; Detection regex handling
(defmessage update-detection-regex (bot))
(defreply update-detection-regex ((bot (proto 'command-bot)))
  (setf (detection-regex bot)
        (create-scanner (build-string "^(?:~A[:,] |~A)" (nickname bot) (command-prefix bot))
                        :case-insensitive-mode T)))

(defreply (setf command-prefix) :after (new-value (bot (proto 'command-bot)))
  "Keeps the detection-regex up-to-date."
  (declare (ignore new-value))
  (update-detection-regex bot))

(defreply (setf nickname) :after (new-value (bot (proto 'command-bot)))
  "Keeps the detection-regex up-to-date."
  (declare (ignore new-value))
  (update-detection-regex bot))

;;; Command handling stuff
(define-condition unknown-command (error)
  ((command-name :reader command-name
                 :initarg :command-name))
  (:report (lambda (condition stream)
             (format stream "Unknown command: ~A"
                     (command-name condition)))))

(defmessage add-command (bot name command))
(defmessage remove-command (bot command))
(defmessage find-command (bot name))
(defmessage command-function (bot command))
(defmessage erase-all-commands (bot))
(defmessage list-all-commands (bot))

(defreply add-command ((bot (proto 'command-bot)) name command)
  (setf (gethash name (commands bot)) command))

(defreply remove-command ((bot (proto 'command-bot)) name)
  (remhash name (commands bot)))

(defreply find-command ((bot (proto 'command-bot)) name)
  (with-properties (commands) bot
    (gethash (string-downcase name) commands)))

(defreply command-function ((bot (proto 'command-bot)) name)
  (or (let ((cmd (find-command bot name)))
	(when cmd (cmd-function cmd)))
      (error 'unknown-command :command-name name)))

(defreply erase-all-commands ((bot (proto 'command-bot)))
  (clrhash (commands bot)))

(defreply list-all-commands ((bot (proto 'command-bot)))
  (with-properties (commands) bot
    (hash-table-keys commands)))

;; A very convenient macro...
(defmacro defcommand (name (&optional (regex "") &rest vars) &body body)
  (let ((documentation nil)
	(real-body nil))
    (if (and (stringp (car body))
	     (cdr body))
	(progn
	  (setf documentation (car body))
	  (setf real-body (cdr body)))
	(setf real-body body))
    `(add-command (proto 'command-bot) (string-downcase (symbol-name ',name))
		  (defclone ((proto 'command))
		      ((cmd-function
                        (lambda ()
                          ,@(if vars
                                `((or (register-groups-bind ,vars (,regex *message*)
                                        ,@real-body)
                                      (error ,(build-string "Not enough arguments to ~A. Try 'help ~:*~A'"
                                                            name))))
                                `(,@real-body))))
		       ,@(when documentation
			       `((dox ,documentation))))))))

;;; Command processing
(defmessage get-message-index (bot message)
  (:documentation "Checks if a message is applicable for the bot. If so,
it returns the command section of the message."))
(defreply get-message-index ((bot (proto 'sykobot)) message)
  (nth-value 1 (scan (detection-regex bot) message)))

(deflistener command-listener
  "When a message is applicable for the bot, respond to it."
  (let ((index (get-message-index *bot* *message*)))
    (when index
      (restartable (respond-to-message *bot* *sender* *channel*
				       (subseq *message* index))))))

(defmessage respond-to-message (bot sender channel message))
(defmessage get-responses (bot cmd args sender channel))
(defmessage process-command-string (bot string sender channel &optional pipe-input))

(defreply respond-to-message ((bot (proto 'sykobot))
                              (sender (proto 'string))
                              (channel (proto 'string))
                              (message (proto 'string)))
  "Removes the direct message indicator from a message, and then
splits it into a command and arguments"
  (destructuring-bind (command &optional *message*)
      (split "\\s+" message :limit 2)
    (send-reply bot channel sender
                (funcall (command-function bot command))))
