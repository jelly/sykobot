;;;; Copyright 2009 Josh Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

(defvar *active-bot* nil)

(defreply init-bot :around ((bot (proto 'irc-bot)))
  (when *active-bot*
    (cerror "Disconnect the current bot."
            "There is already a bot running.")
    (disconnect *active-bot* "Restarting bot"))
  (call-next-reply)
  (setf *active-bot* bot))