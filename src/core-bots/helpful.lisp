(in-package :sykobot)

(defproto helpful-bot ((proto 'irc-bot))
  ((docstrings (make-hash-table :test #'equalp))))

(defmessage stored-docstring (bot thing))
(defmessage (setf stored-docstring) (new-docstring bot thing))

(defreply stored-docstring ((bot (proto 'helpful-bot)) (thing (proto 'string)))
  (gethash thing (docstrings bot)))

(defreply (setf stored-docstring) ((new-docstring (proto 'string)) (bot (proto 'helpful-bot)) (thing (proto 'string)))
  (setf (gethash thing (docstrings bot)) new-docstring))

(defreply (setf stored-docstring) :around ((new-docstring (proto 'string)) (bot (proto 'helpful-bot)) (thing (proto 'string)))
  (let ((old-docstring (stored-docstring bot thing)))
    (when old-docstring
      (warn "Clobbering old docstring \"~A\" for thing: ~A ~
             with new docstring \"~A\""
            old-docstring thing new-docstring))
    (call-next-reply)))

(defproto helpful nil
  ((name "" :reader name)
   (docstring "There is no documentation here yet.")))

(defreply (setf docstring) :after ((new-docstring (proto 'string)) (thing (proto 'helpful)))
  (setf (stored-docstring (proto 'helpful-bot) (name thing)) new-docstring))

(defreply init-sheep :after ((documented-sheep (proto 'helpful)) &key)
  (with-properties (name docstring) documented-sheep
    (setf (stored-docstring (proto 'helpful-bot) name) docstring)))
