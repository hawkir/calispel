(in-package #:calispel)

(defclass alt-operation (operation)
  ((action :type function :initarg :action :reader action
	   :initform (error "Must supply :ACTION.")
           :documentation "A function to be called when this OPERATION
succeeds.

When DIRECTION is SEND, the function is called with no arguments.

When DIRECTION is RECEIVE, the function is called with the received
value.

The result of this function is the result of the ALT macro form."))
  (:documentation "An OPERATION with bookkeeping for use by the *-ALT
macros."))

(defun invoke-action (op)
  "Invokes the action associated with the given ALT-OPERATION."
  (declare (type alt-operation op))
  (ecase (direction op)
    (send (funcall (action op)))
    (receive (funcall (action op) (value op)))))

;;; Macros.

(defmacro fair-alt (&body clauses)
  "Performs one of the given channel operations, choosing fairly from
the set of operations that first becomes available, then evaluates
each of the forms associated with the selected operation.  If no
operation can immediately be made, waits until an operation is
available (optionally up to a given timeout).  The result is the
result of the final evaluated form (or no values if no clause was
executed).

clauses          ::= operation-clause* [otherwise-clause]
operation-clause ::= (operation form*)
otherwise-clause ::= ({otherwise | (otherwise [:timeout timeout])} form*)
operation        ::= (? channel [lambda-list [condition]]) ; receive
                   | (! channel value [condition])         ; send

channel: Evaluated to produce a CHANNEL to send to or receive from.
The channel forms associated with operations that do not pass the
condition are not evaluated.

lambda-list: Either a symbol naming a variable to be bound to the
value received from the channel, or a destructuring lambda list naming
a set of variables to be bound to the destructured value received from
the channel.  The bindings are visible to the associated forms.  If
the value cannot be destructured according to the lambda list, an
error is signalled.  Note that multiple receive clauses for the same
channel with different destructuring lambda-lists *cannot* be used for
pattern matching.

value: An expression whose primary value is used as the message to
send to the channel.  All value expressions are evaluated before
selecting an operation, except for those associated with operations
that do not pass the condition.

condition: Evaluated to produce a generalized boolean indicating
whether the associated operation-clause should receive further
consideration.  When condition is not given or its resulting value is
true, the associated operation is kept for consideration.  When the
resulting value is false, the operation is removed from
consideration (as if its associated channel never becomes ready for
sending/receiving).

form: Evaluated in sequence when the associated clause is executed.
The values of the evaluation of the last form of the effective clause
become the result of FAIR-ALT.

timeout: Evaluated to produce the duration, as a non-negative REAL
number of seconds, to wait for an effective operation to become
available before resorting to the otherwise-clause.  The result may
also be NIL to specify no time out.  When an otherwise-clause exists,
the default time out is 0, meaning that if none of the channels in the
operation-clauses are immediately available, the otherwise-clause
forms are executed immediately.  When there is no otherwise-clause,
the default time out is NIL.

It is useful to specify a timeout expression that conditionally
evaluates to NIL, in order to disable the time out and inhibit the
execution of the otherwise-clause (provided that there are channel
operations to wait for that haven't been excluded by a false
condition).

If there are no effective operations (because all the conditions
evaluated to false, or because no operations were specified), then the
otherwise-clause (if any) is executed immediately (even if the
specified time out is NIL).

Stylistically and for future compatibility, avoid side-effects in
channel, value, condition, and timeout expressions."
  (alt-code clauses :fair))

(defmacro pri-alt (&body clauses)
  "Performs one of the given channel operations, choosing the first
listed operation that becomes available, then evaluates each of the
forms associated with the selected operation.  If no operation can
immediately be made, waits until an operation is available (optionally
up to a given timeout).  The result is the result of the final
evaluated form (or no values if no clause was executed).

The syntax and semantics (other than clause priority) are the same as
with FAIR-ALT.  PRI-ALT is (currently) more efficient than FAIR-ALT."
  (alt-code clauses :first))

(defun alt-code (clauses priority)
  (let* ((otherwise-pos (position-if #'otherwise-clause? clauses))
	 (otherwise (unless (null otherwise-pos)
		      (elt clauses otherwise-pos)))
	 (ops (loop for clause in clauses
		    for i from 0
		    unless (and (not (null otherwise-pos))
				(= i otherwise-pos))
		    collecting clause)))
    (unless (or (null otherwise-pos)
                (= otherwise-pos (1- (length clauses))))
      ;; Also takes care of ensuring only one else-clause.
      (error "Optional OTHERWISE clause must come last, and only once."))
    (multiple-value-bind (timeout-form otherwise-forms)
	(if (null otherwise)
	    (values nil nil)
	    (parse-otherwise-clause otherwise))
      (alt-body-code (map 'list #'op-clause-condition ops)
		     (map 'list #'op-clause-form ops)
		     priority
		     (not (null otherwise-pos)) timeout-form
		     otherwise-forms))))

(defun alt-body-code (op-conditions op-forms priority
		      otherwise-p timeout-form otherwise-forms)
  (jpl-util:with-gensyms (ops% timeout% result%)
    `(let* ((,ops% (let ((,ops% '()))
		     ,@(loop for condition in (reverse op-conditions)
			     for form in (reverse op-forms)
			     collecting `(when ,condition
					   (push ,form ,ops%)))
		     ,ops%))
	    (,timeout%
	     ;; Bind in new variable to give the user a pretty place
	     ;; name ("TIMEOUT") rather than an ugly one
	     ;; ("TIMEOUT%-1234").  (SBCL will include the place-name
	     ;; in the printed condition.)
	     (let ((timeout ,timeout-form))
	       ;; Use CHECK-TYPE rather than JPL-UTIL:ENSURE-TYPE to
	       ;; give the compiler a chance to optimize the check
	       ;; away.
	       (check-type timeout (or (real 0) null)
			   "a non-negative REAL number of seconds (or NIL)")
	       timeout))
	    (,result% (unless (endp ,ops%)
			(operation-alternate ,timeout% ,priority ,ops%))))
       (cond ((null ,result%)
	      ,@(if otherwise-p
		    otherwise-forms
		    '((values))))
	     (t (invoke-action ,result%))))))

(defun otherwise-clause? (clause)
  (and (listp clause)
       (not (endp clause))
       (or (eq 'otherwise (first clause))
	   (and (listp (first clause))
		(not (endp (first clause)))
		(eq 'otherwise (first (first clause)))))))

(defun parse-otherwise-clause (clause)
  (declare (type list clause))
  (destructuring-bind (head &body body) clause
    (if (eq head 'otherwise)
	(values 0 body)
	(destructuring-bind (head &key (timeout 0)) head
	  (assert (eq head 'otherwise))
	  (values timeout body)))))

(defun op-clause-condition (clause)
  (declare (type list clause))
  (destructuring-bind ((operator &rest operands) &body body) clause
    (declare (ignore body))
    (ecase operator
      (! (op-!-clause-condition operands))
      (? (op-?-clause-condition operands)))))

(defun op-!-clause-condition (clause-operands)
  (declare (type list clause-operands))
  (destructuring-bind (channel-form value-form &optional (condition-form t))
      clause-operands
    (declare (ignore channel-form value-form))
    condition-form))

(defun op-?-clause-condition (clause-operands)
  (declare (type list clause-operands))
  (destructuring-bind (channel-form &optional lambda-list (condition-form t))
      clause-operands
    (declare (ignore channel-form lambda-list))
    condition-form))

(defun op-clause-form (clause)
  (declare (type list clause))
  (destructuring-bind ((operator &rest operands) &body body) clause
    (ecase operator
      (! (op-!-clause-form operands body))
      (? (op-?-clause-form operands body)))))

(defun op-!-clause-form (clause-operands body)
  (declare (type list clause-operands body))
  (destructuring-bind (channel-form value-form &optional condition-form)
      clause-operands
    (declare (ignore condition-form))
    `(make-instance 'alt-operation
                    :direction 'send
                    :channel ,channel-form
                    :value ,value-form
                    :action (lambda () ,@body))))

(defun op-?-clause-form (clause-operands body)
  (declare (type list clause-operands body))
  (destructuring-bind (channel-form &optional
		       (lambda-list nil lambda-list-p)
		       condition-form)
      clause-operands
    (declare (ignore condition-form))
    (jpl-util:with-gensyms (message%)
      (let ((action-expr
	     (cond ((not lambda-list-p)
		    `(lambda (,message%)
		       (declare (ignore ,message%))
		       ,@body))
		   ((listp lambda-list)
		    `(lambda (,message%)
		       (destructuring-bind ,lambda-list ,message%
			 ,@body)))
		   ((symbolp lambda-list)
		    `(lambda (,lambda-list)
		       ,@body))
		   (t (error "LAMBDA-LIST of receive clause must be ~
                              a list, a symbol, or unspecified, not ~S."
			     lambda-list)))))
	`(make-instance 'alt-operation
			:direction 'receive
			:channel ,channel-form
			:action ,action-expr)))))
