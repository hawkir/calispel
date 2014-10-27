(in-package #:calispel)

(defun ? (channel &optional timeout)
  "Receive a value from CHANNEL, waiting up to TIMEOUT seconds (a
non-negative REAL number; or indefinitely when NIL).  Returns the
value (or NIL upon timeout) and a boolean indicating whether the
timeout expired before a value could be received."
  (let* ((op (make-instance 'operation :channel channel :direction 'receive))
	 (chosen (operation-alternate timeout :first (list op))))
    (if (null chosen)
	(values nil nil)
	(values (value chosen) t))))

(defun ! (channel value &optional timeout)
  "Send VALUE on CHANNEL, waiting up to TIMEOUT seconds (a
non-negative REAL number; or indefinitely when NIL).  Returns a
boolean indicating whether the timeout expired before the value could
be sent."
  (let* ((op (make-instance 'operation :channel channel :direction 'send
			    :value value))
	 (chosen (operation-alternate timeout :first (list op))))
    (not (null chosen))))
