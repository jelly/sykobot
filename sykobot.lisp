(defpackage #:sykobot
  (:use :cl :cl-ppcre :sheeple))
(in-package :sykobot)

(defproto sykobot ()
  ((connection nil)
   (msg-loop-thread nil)
   (nickname "sykobot2")
   (silentp nil)))

;;;
;;; IRC connection
;;;
(defmessage connect (bot server))
(defmessage disconnect (bot &optional message))
(defmessage join (bot channel))
(defmessage part (bot channel))
(defmessage identify (bot password))

(defreply connect ((bot #@sykobot) server)
  (setf (connection bot) (irc:connect :nickname (nickname bot) :server server))
  (irc:add-hook (connection bot) 'irc:irc-privmsg-message (lambda (msg)
                                                            (msg-hook bot msg)))
  (setf (msg-loop-thread bot) (bt:make-thread
                               (lambda ()
                                 (irc:read-message-loop (connection bot))))))

(defreply disconnect ((bot #@sykobot) &optional message)
  (bt:destroy-thread (msg-loop-thread bot))
  (irc:quit (connection bot) (or message (values))))

(defreply join ((bot #@sykobot) channel)
  (irc:join (connection bot) channel))

(defreply part ((bot #@sykobot) channel)
  (irc:part (connection bot) channel))

(defreply identify ((bot #@sykobot) password)
  (send-msg bot "nickserv" (format nil "identify ~A" password)))

;;;
;;; irc functions
;;;
(defmessage nick (bot new-nick))
(defmessage send-notice (bot target message))
(defmessage send-msg (bot channel message))
(defmessage topic (bot channel &optional new-topic))

(defreply nick ((bot #@sykobot) new-nick)
  (setf (nickname bot) new-nick)
  (irc:nick (connection bot) new-nick))

(defreply send-notice ((bot #@sykobot) target message)
  (irc:notice (connection bot) target message))

(defreply send-msg ((bot #@sykobot) channel message)
  (irc:privmsg (connection bot) channel (or message "")))

(defreply topic ((bot #@sykobot) channel &optional new-topic)
  (if new-topic
      (irc:topic- (connection bot) channel new-topic)
      (irc:topic (irc:find-channel (connection bot) channel))))

;;;
;;; Message processing
;;;
(defmessage msg-hook (bot msg))
(defreply msg-hook ((bot #@sykobot) msg)
  (let ((sender (irc:source msg))
        (channel (car (irc:arguments msg)))
        (message (second (irc:arguments msg))))
    (if (silentp bot)
        (silent-mode-process-message bot sender channel message)
        (process-message bot sender channel message))))

(defmessage shut-up (bot))
(defreply shut-up ((bot #@sykobot))
  (setf (silentp bot) t))
(defmessage un-shut-up (bot))
(defreply un-shut-up ((bot #@sykobot))
  (setf (silentp bot) nil))

(defmessage silent-mode-process-message (bot sender channel message))
(defreply silent-mode-process-message ((bot #@sykobot) sender channel message)
  (when (sent-to-me-p bot channel message)
    (let ((command (car (split "\\s+" (scan-string-for-direct-message bot channel message) :limit 2))))
      (when (string-equal command "talk")
        (send-msg bot channel (format nil "~A: bla bla bla bla. There, happy?" sender))
        (un-shut-up bot)))))

(defmessage process-message (bot sender channel message))
(defreply process-message ((bot #@sykobot) sender channel message)
  (when (sent-to-me-p bot channel message)
    (respond-to-message bot sender channel message))
  (when (and (has-url-p message)
             (not (string-equal sender (nickname bot))))
    (handler-case
        (multiple-value-bind (title url)
            (url-info (grab-url message))
          (send-msg bot channel (format nil "Title: ~A (at ~A)" title (puri:uri-host (puri:uri url)))))
      (error ()
        (values)))))

(defmessage respond-to-message (bot sender channel message))
(defreply respond-to-message ((bot #@sykobot) sender channel message)
  (let* ((string (scan-string-for-direct-message bot channel message))
         (command+args (split "\\s+" string :limit 2)))
    (handler-case
        (answer-command bot (car command+args) (cadr command+args) sender channel)
      (error (e)
        (send-msg bot channel (format nil "~A: An error occurred: ~A" sender e))))))

(defmessage answer-command (bot cmd args sender channel))
(defreply answer-command ((bot #@sykobot) cmd args sender channel)
  (let ((fn (command-function cmd)))
    (funcall fn bot args sender channel)))

(defun sent-to-me-p (bot channel message)
  (when (scan-string-for-direct-message bot channel message)
    t))

(defmessage scan-string-for-direct-message (bot channel message))
(defreply scan-string-for-direct-message ((bot #@sykobot) channel message)
  (cond ((equal channel (nickname bot))
         message)
        ((scan (format nil "^~A: " (nickname bot)) message)
         (regex-replace (format nil "^~A: " (nickname bot)) message ""))
        ((scan (format nil "^~A, " (nickname bot)) message)
         (regex-replace (format nil "^~A, " (nickname bot)) message ""))
        ((scan "^!+" message)
         (regex-replace "^!+" message ""))))
