;;;; Copyright 2009 Josh Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

;;; These are only bound within the body of listeners.
(defvar *bot*)
(defvar *message*)
(defvar *sender*)
(defvar *channel*)

;;; Must be bound before the bot runs
(defvar *default-listeners*)
(defvar *default-listeners-by-channel*)

;;; Modularization
(defproto listener-bot ((proto 'helpful-bot))
  ((listeners (make-hash-table :test #'eq))
   (active-listeners nil)
   (deafp nil)))

(defproto listener ((proto 'helpful))
  ((name "Listener" :reader name)
   (docstring "The listener system is like my backbone")
   (code-fn (constantly nil))))

;;; Handling of listeners
(defreply msg-hook ((*bot* (proto 'listener-bot)) msg)
  (let ((*sender* (irc:source msg))
        (*channel* (let ((target (car (irc:arguments msg))))
		   (if (equal target (nickname *bot*))
		       (irc:source msg)
		       target)))
        (*message* (escape-format-string (cadr (irc:arguments msg)))))
    (handler-case
        (call-active-listeners *bot* *channel*)
      (error (e) (send-msg *bot* *channel*
                           (build-string "ERROR: ~A" e))))))

(defmessage set-listener (bot listener))
(defmessage remove-listener (bot name))
(defmessage call (bot name))

(defreply set-listener ((bot (proto 'listener-bot)) (listener (proto 'listener)))
  (setf (gethash (name listener) (listeners bot)) listener))

(defreply remove-listener ((bot (proto 'listener-bot)) (name (proto 'symbol)))
  (remhash name (listeners bot)))

(defreply call-listener ((bot (proto 'listener-bot)) (listener (proto 'listener)))
  (declare (ignore bot))
  (funcall (code-fn listener)))
(defreply call-listener ((bot (proto 'listener-bot)) (listener (proto 'symbol)))
  (call-listener bot (gethash listener (listeners bot))))

(defmacro deflistener (name &body body)
  (let ((documentation nil))
    (when (and (stringp (car body))
               (cadr body))
      (setf documentation (pop body)))
    `(set-listener (proto 'listener-bot)
                   (defclone ((proto 'listener))
                       ((docstring
                         ,(or documentation "This feature is undocumented."))
                        (name ,(symbol-name name))
                        (code-fn (lambda () ,@body)))
                     (:nickname ',name)))))

;;; Customization of listeners
(defmessage listener-on (bot channel name))
(defmessage listener-off (bot channel name))
(defmessage call-active-listeners (bot channel))
(defmessage listener-active-p (bot channel name))

(defreply listener-on ((bot (proto 'listener-bot)) channel (listener (proto 'listener)))
  (pushnew listener (alref channel (active-listeners bot))))
(defreply listener-on ((bot (proto 'listener-bot)) channel (name (proto 'symbol)))
  (listener-on bot channel (gethash name (listeners bot))))

(defreply listener-off ((bot (proto 'listener-bot)) channel (listener (proto 'listener)))
  (setf (alref channel (active-listeners bot))
        (delete listener (alref channel (active-listeners bot)))))
(defreply listener-off ((bot (proto 'listener-bot)) channel (name (proto 'symbol)))
  (listener-off bot channel (gethash name (listeners bot))))

(defreply call-active-listeners ((bot (proto 'listener-bot)) channel)
  (let ((deafp (alref channel (deafp bot))))
    (if deafp
        (call-listener bot deafp)
        (dolist (name (alref channel (active-listeners bot)))
          (call-listener bot name)))))

(defreply listener-active-p ((bot (proto 'listener-bot)) channel (listener (proto 'listener)))
  (member listener (alref channel (active-listeners bot))))
(defreply listener-active-p ((bot (proto 'listener-bot)) channel (name (proto 'symbol)))
  (listener-active-p bot channel (gethash name (listeners bot))))

(defun activate-listeners (bot channel &rest names)
  (dolist (name names)
    (listener-on bot channel name)))

(defreply part :after ((bot (proto 'listener-bot)) channel)
  (setf (alref channel (active-listeners bot)) nil))

(defreply join :after ((bot (proto 'listener-bot)) channel)
  (let ((channel-listeners (alref channel *default-listeners-by-channel*)))
    (if channel-listeners
	(apply #'activate-listeners bot channel channel-listeners)
	(apply #'activate-listeners bot channel *default-listeners*))))

;;; Deafness (aka silence)
(defmessage toggle-deafness (bot channel))

(defreply toggle-deafness ((bot (proto 'listener-bot)) channel)
  (setf (alref channel (deafp bot))
        (and (not (deafp bot))
             'undeafen)))

(deflistener undeafen
  (unless (zerop (length *message*))
    (toggle-deafness *bot* *channel*)))