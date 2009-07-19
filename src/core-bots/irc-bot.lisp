;;;; Copyright 2009 Josh Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

(defproto irc-bot ()
  ((connection nil)
   (msg-loop-thread nil)
   (nickname "irc-bot")
   (username "irc-bot")
   (realname "sykobot")
   (server "irc.freenode.net")
   (port 6667)
   (dir "default-bot/")
   (password nil)
   (channels nil))
  (:documentation
   "This proto, and the behavior associated with it, provide an IRC
    API which attempts to abstract over CL-IRC."))

;;; IRC connection
(defmessage init-bot (bot))
(defmessage connect (bot))
(defmessage disconnect (bot &optional message))
(defmessage join (bot channel))
(defmessage part (bot channel))
(defmessage identify (bot password))

(defreply init-bot ((bot (proto 'irc-bot)))
  (connect bot))

(defreply connect ((bot (proto 'irc-bot)))
  (setf (connection bot) (irc:connect :nickname (nickname bot)
                                      :server (server bot)
                                      :port (port bot)
                                      :password (password bot)
                                      :username (username bot)
                                      :realname (realname bot)))
  (setf (irc:client-stream (connection bot)) (make-broadcast-stream))
  (irc:add-hook (connection bot) 'irc:irc-privmsg-message
                (lambda (msg)
                  (handler-bind ((cl-irc:no-such-reply (lambda (c)
                                                         (let ((r (find-restart 'continue c)))
                                                           (when r (invoke-restart r))))))
                    (msg-hook bot msg))))
  (setf (msg-loop-thread bot)
        (bt:make-thread
         (lambda ()
           (handler-bind ((cl-irc:no-such-reply
                           (lambda (c)
                             (let ((r (find-restart 'continue c)))
                               (when r (invoke-restart r))))))
             (irc:read-message-loop (connection bot)))))))

(defreply disconnect ((bot (proto 'irc-bot)) &optional message)
  (bt:destroy-thread (msg-loop-thread bot))
  (setf *active-bot* nil)
  (irc:quit (connection bot) (or message (values))))

(defreply join ((bot (proto 'irc-bot)) channel)
  (irc:join (connection bot) channel)
  (str-pushnew channel (channels bot)))

(defreply part ((bot (proto 'irc-bot)) channel)
  (irc:part (connection bot) channel)
  (with-accessors ((channels channels)) bot
    (setf channels (str-delete channel channels))))

(defreply identify ((bot (proto 'irc-bot)) password)
  (send-msg bot "nickserv" (build-string "identify ~A" password)))

;;; IRC functions
(defmessage nick (bot new-nick))
(defmessage send-msg (bot target message))
(defmessage send-reply (bot target user message))
(defmessage send-action (bot channel action))
(defmessage topic (bot channel &optional new-topic))
(defmessage whois (bot mask))

(defreply nick ((bot (proto 'irc-bot)) new-nick)
  (setf (nickname bot) new-nick)
  (irc:nick (connection bot) new-nick))

(defreply send-msg ((bot (proto 'irc-bot))
                    (target (proto 'string))
                    (message (proto 'string)))
  (with-properties (connection) bot
    (do-lines (line message collected-lines)
      do (irc:privmsg connection
		      target
		      ;; The following prevents the injection of arbitrary raw IRC via messages containing \r and other possibly meaningful non-printable characters in cases where raw message content originates from a third party source, e.g. raw URL title echoing.
		      (remove-if
		       (lambda (c) (< (char-code c) 32))
		       line))
      collect line into collected-lines)))

(defreply send-reply ((bot (proto 'irc-bot))
                      (target (proto 'string))
                      (user (proto 'string))
                      (message (proto 'string)))
  (send-msg bot target
            (if (string-equal target user) message
                (apply #'merge-strings #\Newline
                       (do-lines (line (build-string message) message)
                         collect (build-string "~A: ~A"
					       user line)
			 into message)))))

(defreply send-action ((bot (proto 'irc-bot)) channel action)
  (irc:privmsg (connection bot) channel
               (build-string "~AACTION ~A~2:*~A" #\^A action)))

(defreply topic ((bot (proto 'irc-bot)) channel &optional new-topic)
  (if new-topic
      (irc:topic- (connection bot) channel new-topic)
      (irc:topic (irc:find-channel (connection bot) channel))))

(defreply whois ((bot (proto 'irc-bot)) mask)
  (irc:whois (connection bot) mask))

;;; Message processing doesn't happen in (proto 'irc-bot)!!!
(defmessage msg-hook (bot msg))
(defreply msg-hook ((bot (proto 'irc-bot)) msg)
  (declare (ignore bot msg)))