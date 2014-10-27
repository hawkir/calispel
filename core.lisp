(in-package #:calispel)

;;; DIRECTION type.

(deftype direction ()
  '(member send receive))

(defun opposite-direction (direction)
  (declare (type direction direction))
  (ecase direction
    (send 'receive)
    (receive 'send)))

;;; CHANNEL class.

(defclass channel ()
  ((buffer :type jpl-queues:queue :reader buffer
	   :initarg :buffer :initform +null-queue+
	   :documentation "The QUEUE used to buffer pending objects.

The QUEUE must not be holding any objects, and the QUEUE must not be
used again unless the CHANNEL owning it is never used
again.  (Exception: QUEUEs that strictly have no state, such as
instances of NULL-QUEUE, may be shared among CHANNELs.)")
   (send-operation-queue :type jpl-queues:queue
			 :reader send-operation-queue
			 :initform (make-instance
				    'jpl-queues:unbounded-random-queue)
			 :documentation "A queue of all the
OPERATIONs waiting to send to this CHANNEL.  An OPERATION may be
waiting to send only when BUFFER is full.")
   (receive-operation-queue :type jpl-queues:queue
			    :reader receive-operation-queue
			    :initform (make-instance
				       'jpl-queues:unbounded-random-queue)
			    :documentation "A queue of all the
OPERATIONs waiting to receive from this channel.  An OPERATION may be
waiting to receive only when BUFFER is empty."))
  (:documentation "A communication channel."))

(defmethod initialize-instance :after ((channel channel) &key &allow-other-keys)
  (unless (jpl-queues:empty? (buffer channel))
    (error "The BUFFER of CHANNEL has objects in queue: ~S"
	   (buffer channel))))

(defmethod print-object ((channel channel) stream)
  (print-unreadable-object (channel stream :type t :identity t)
    (format stream "buffer: ~S" (buffer channel))))

(defun operation-queue (channel direction)
  "Returns the queue of all the OPERATIONs waiting to move data in
DIRECTION on CHANNEL.  When DIRECTION is SEND, the returned OPERATIONs
are those waiting until the BUFFER of CHANNEL is no longer full.  When
RECEIVE, the returned OPERATIONs are those waiting until the BUFFER is
no longer empty."
  (declare (type channel channel)
	   (type direction direction))
  (ecase direction
    (send (send-operation-queue channel))
    (receive (receive-operation-queue channel))))

;;; Global lock.
;;; 
;;; This lock protects all channel state, globally.  The original code
;;; has this to say:
;;; 
;;;     One can go through a lot of effort to avoid this global lock.
;;;     You have to put locks in all the CHANNELs and all the
;;;     OPERATIONs.  at the beginning of OPERATION-ALTERNATE you have
;;;     to lock all the CHANNELs, but then to try to actually execute
;;;     an OPERATION you have to lock the other guy's OPERATIONs, so
;;;     that other people aren't trying to use him in some other
;;;     alternation at the same time.
;;;     
;;;     It's just not worth the extra effort.
;;; 
;;; At least one special case has to be avoided (as alluded to above):
;;; 
;;;   * Thread 1 is blocking on an ALTERNATION with two OPERATIONs.
;;;     Operation 1 is to receive from Channel 1.  Operation 2 is to
;;;     receive from Channel 2.
;;;   
;;;   * Thread 2 is in OPERATION-ALTERNATE with one operation
;;;     (Operation 3): send to Channel 1.
;;;   
;;;   * Thread 3 is in OPERATION-ALTERNATE with one operation
;;;     (Operation 4): send to Channel 2.
;;;   
;;;   * Thread 2 and Thread 3 are running simultaneously.
;;;   
;;;   * It must be guaranteed that only either O1 or O2 is selected
;;;     for the alternation in T1.
;;; 
;;; Now let's play out what happens:
;;; 
;;;   * At the same time, T2 dequeues O1 from C1's receiving operation
;;;     queue, and T3 dequeues O2 from C2's receiving operation queue.
;;; 
;;; So, we need some magical (and probably quite complicated) way of
;;; locking just enough of the data structures to prevent the conflict
;;; in the last bullet point, or we need a way for one thread to see
;;; there's a conflict and "retry" back to OPERATION-ALTERNATE,
;;; looking for another ready operation (also probably quite
;;; complicated).
;;; 
;;; Nomatter how you slice it, the solution will be hard, and proving
;;; its correctness even harder.  What performance gain (if any, with
;;; all that fine-grained locking) do you get?  Is it worth it?
;;; 
;;; And finally, you'd also have to give consideration to
;;; *RANDOM-STATE*, to ensure the current RANDOM-STATE (shared among
;;; threads) won't be accessed and updated by RANDOM concurrently.
;;; RANDOM is used directly in this library, and in JPL-QUEUES.
(defvar *lock*
  (bt:make-lock)
  "A lock protecting the global channel state.  The lock must be held
whenever any data is being accessed (unless it can be proven that no
other thread can access that data).  Specifically, that means
CHANNELs, OPERATIONs, and ALTERNATIONs that other threads can
potentially get access to.")

;;; OPERATION class.

(defclass operation ()
  ((direction :type direction :initarg :direction :reader direction
              :initform (error "Must supply :DIRECTION.")
              :documentation "Which DIRECTION this OPERATION is trying
to move data in.

When SEND, the OPERATION is interested in sending the value specified
by :VALUE to CHANNEL.

When RECEIVE, the OPERATION is interested in receiving a value from
CHANNEL.")
   (channel :type channel :initarg :channel :reader channel
            :initform (error "Must supply :CHANNEL.")
            :documentation "The CHANNEL this OPERATION is interested
in operating on.")
   (value :initarg :value :accessor value
          :documentation "The value associated with this OPERATION.

When sending, this is the value to send.

When receiving, this is the received value if the OPERATION has
executed, or undefined if it has not.")
   (alternation :type alternation :accessor alternation
		:documentation "The ALTERNATION (if any) that this
OPERATION is a member of."))
  (:documentation "A potential operation (receive or send) to perform
on a channel.  An OPERATION instance represents an interest to perform
the operation; it does not represent an operation that definitely will
be or has been carried out."))

;;; ALTERNATION class.

(defclass alternation ()
  ((operations :type list :reader operations :initarg :operations
	       :initform (error "Must supply :OPERATIONS.")
	       :documentation "The set of OPERATIONs waiting to
occur (as a list).")
   (selected :type (or operation null) :accessor selected :initform nil
	     :documentation "The OPERATION selected by a thread that
took action, or NIL if no OPERATION has yet been executed by another
thread.

The thread that writes to SELECTED is generally a different thread
than that which waits on the ALTERNATION.

The OPERATION, when given, must have been executed, and it must appear
in the OPERATIONS slot.")
   (selection-cv :reader selection-cv :initform (bt:make-condition-variable)
		 :documentation "A condition variable which is
notified when an OPERATION has been selected and was written to the
SELECTED slot.

The thread that waits on SELECTION-CV is generally that which is
waiting on the ALTERNATION."))
  (:documentation "Represents a waiting alternation of several
OPERATIONs.  That is, represents the act of waiting for the associated
OPERATION that first becomes available."))

;;; OPERATION functions.

(defun operation-alternate (timeout priority ops)
  "Given a list of at least one OPERATION, executes the first one that
becomes available within TIMEOUT seconds and returns that OPERATION.
If TIMEOUT seconds have elapsed without any of the OPERATIONs becoming
available, returns NIL.  If TIMEOUT is NIL, waits indefinitely.

If one or more of the OPERATIONs can be executed immediately, which
one is chosen depends on the value of PRIORITY.  When PRIORITY
is :FIRST, the first OPERATION listed in OPS that can be executed is
chosen.  When PRIORITY is :FAIR, one of the OPERATIONs that can be
executed immediately is chosen at random."
  (declare (type (or (real 0) null) timeout)
	   (type (member :first :fair) priority)
	   (type list ops))
  (when (endp ops)
    (error "At least one OPERATION must be given."))
  (bt:with-lock-held (*lock*)
    ;; Seek an immediately-executable operation.
    (let ((ready-op
	   (ecase priority
	     (:first (find-if #'operation-ready? ops))
	     (:fair
	      ;; If we want to ridiculously microoptimize for the many
	      ;; ready OPERATIONs case, we could use reservoir
	      ;; sampling to avoid consing up a list of all ready
	      ;; OPERATIONs and scanning over it twice.
	      (let ((ready-ops (remove-if (complement #'operation-ready?)
					  ops)))
		(unless (endp ready-ops)
		  (elt ready-ops (random (length ready-ops)))))))))
      (cond ((not (null ready-op))
	     (execute-operation ready-op)
	     ready-op)
	    ((or (null timeout)
		 (plusp timeout))
	     (let ((alt (make-instance 'alternation :operations ops)))
	       (dolist (op ops)
		 (setf (alternation op) alt))
	       (alternation-wait timeout alt)
	       (selected alt)))
	    (t nil)))))

(defun alternation-wait (timeout alternation)
  "Given an ALTERNATION, waits up to TIMEOUT seconds for another
thread to execute one of its OPERATIONs (or indefinitely when TIMEOUT
is NIL).  The SELECTED slot of ALTERNATION must initially be NIL.

Upon return, if another thread executed one of the OPERATIONs of
ALTERNATION, that OPERATION will appear in the SELECTED slot of
ALTERNATION.  Otherwise (if timed-out), that slot will be NIL.

Must be called with *LOCK* held."
  (declare (type (or (real 0) null) timeout)
	   (type alternation alternation))
  (let ((start-time (jpl-util:get-reasonable-real-time)))
    ;; FIXME: this is a hack, and a bit wasteful.  Find or create a
    ;; reasonable timers library that can do this.
    (unless (null timeout)
      ;; EAGER-FUTURE explicitly leaves the dynamic environment
      ;; unpsecified.
      (let ((lock *lock*))
	(bt:make-thread
         (lambda ()
           (sleep timeout)
           (bt:with-lock-held (lock)
             (bt:condition-notify (selection-cv alternation)))))))
    (labels ((elapsed-time ()
	       (- (jpl-util:get-reasonable-real-time)
		  start-time))
	     (timeout-expired? ()
	       (and (not (null timeout))
		    (>= (elapsed-time) timeout))))
      (assert (null (selected alternation)))
      (map nil #'enqueue-operation-with-channel
	   (operations alternation))
      (unwind-protect
	   (loop
	     (bt:condition-wait (selection-cv alternation) *lock*)
	     (unless (null (selected alternation))
	       (return))
	     (when (timeout-expired?)
	       (return)))
	(when (null (selected alternation))
	  (map nil #'dequeue-operation-with-channel
	       (operations alternation)))))))

(defun enqueue-operation-with-channel (op)
  "Given an OPERATION that is about to wait, enqueues it with the
vector of OPERATIONs waiting on CHANNEL (where CHANNEL is the CHANNEL
that the OPERATION is interested in).

Must be called with *LOCK* held."
  (jpl-queues:enqueue op (operation-queue (channel op) (direction op))))

(defun dequeue-operation-with-channel (op)
  "Given an OPERATION that will no longer be waiting, dequeues it from
the vector of OPERATIONs waiting on CHANNEL (where CHANNEL is the
CHANNEL that the OPERATION was interested in).

Must be called with *LOCK* held."
  (jpl-queues:dequeue-object op (operation-queue (channel op) (direction op))))

(defun operation-ready? (op)
  "Returns a boolean value indicating whether the given OPERATION can
be executed.

Must be called with *LOCK* held."
  (declare (type operation op))
  (let ((channel (channel op))
	(direction (direction op)))
    (or (not (jpl-queues:empty? (operation-queue
				 channel (opposite-direction direction))))
	(ecase direction
	  (send (not (jpl-queues:full? (buffer channel))))
	  (receive (not (jpl-queues:empty? (buffer channel))))))))

(defun execute-operation (op)
  "Executes the given OPERATION.  It must be ready (per
OPERATION-READY?).

Must be called with *LOCK* held."
  (declare (type operation op))
  (assert (operation-ready? op))
  ;; Find an OPERATION waiting on the same CHANNEL for communication
  ;; in the opposite direction that we can exchange data with.
  (let ((opposite-ops (operation-queue (channel op)
				       (opposite-direction (direction op)))))
    (cond ((not (jpl-queues:empty? opposite-ops))
           (let ((other-op (jpl-queues:dequeue opposite-ops)))
             (ecase (direction op)
               (receive (enqueue/dequeue-channel-from-op-to-op other-op op))
               (send (enqueue/dequeue-channel-from-op-to-op op other-op)))
	     ;; This needs to be done immediately, not in
	     ;; ALTERNATION-WAIT; another thread could jump in when
	     ;; *LOCK* is released, see these queued OPERATIONs, and
	     ;; try to execute them.  Oops!
	     (map nil #'dequeue-operation-with-channel
		  (operations (alternation other-op)))
	     (setf (selected (alternation other-op)) other-op)
             (bt:condition-notify (selection-cv (alternation other-op)))))
	  ;; If there's no waiting opposite OPERATION, then the queue
	  ;; must be buffered.  Go ahead and enqueue/dequeue from that
	  ;; buffer.
          (t (ecase (direction op)
               (receive (dequeue-channel-for-operation op))
               (send (enqueue-channel-for-operation op)))))))

(defun enqueue/dequeue-channel-from-op-to-op (sending-op receiving-op)
  "Given SENDING-OP (an OPERATION interested in sending to a channel),
and RECEIVING-OP (an OPERATION interested in receiving from the same
channel), enqueues SENDING-OP's object and dequeues an object for
RECEIVING-OP, at the same time.

Must be called with *LOCK* held."
  (declare (type operation sending-op receiving-op))
  (assert (eq 'send (direction sending-op)))
  (assert (eq 'receive (direction receiving-op)))
  (assert (eq (channel sending-op) (channel receiving-op)))
  (let ((channel (channel sending-op)))
    (cond ((jpl-queues:empty? (buffer channel))
	   ;; Since the CHANNEL has no queued objects, it is safe to
	   ;; copy directly.
	   (operation-transfer sending-op receiving-op))
	  (t
	   ;; The CHANNEL has buffered objects.  If we copy directly,
	   ;; SENDING-OP's object will be cutting in front of the
	   ;; older, buffered objects.  To ensure fair order,
	   ;; enqueue/dequeue separately.
	   
	   ;; Dequeue before enqueuing, in-case the buffer is full, in
	   ;; order to make room first.
	   (dequeue-channel-for-operation receiving-op)
	   (enqueue-channel-for-operation sending-op)))))

(defun operation-transfer (sending-op receiving-op)
  "Transfers one object from SENDING-OP to RECEIVING-OP.

SENDING-OP must be interested in sending, and RECEIVING-OP in
receiving.  They must be interested in the same channel, and the
channel's BUFFER must be empty.

Must be called with *LOCK* held."
  (declare (type operation sending-op receiving-op))
  (assert (eq 'send (direction sending-op)))
  (assert (eq 'receive (direction receiving-op)))
  (assert (eq (channel sending-op) (channel receiving-op)))
  (assert (jpl-queues:empty? (buffer (channel sending-op))))
  (setf (value receiving-op) (value sending-op)))

(defun dequeue-channel-for-operation (receiving-op)
  "Dequeues the oldest object from the the BUFFER of the CHANNEL that
RECEIVING-OP is interested in receiving from, storing it in
RECEIVING-OP.

RECEIVING-OP must be interested in receiving.  The CHANNEL must have
at least one object in its BUFFER.

Must be called with *LOCK* held."
  (declare (type operation receiving-op))
  (assert (eq 'receive (direction receiving-op)))
  (let ((buffer (buffer (channel receiving-op))))
    (assert (not (jpl-queues:empty? buffer)))
    (setf (value receiving-op) (jpl-queues:dequeue buffer))))

(defun enqueue-channel-for-operation (sending-op)
  "Enqueues the object stored in SENDING-OP to the BUFFER of the
CHANNEL that SENDING-OP is interested in sending to.

SENDING-OP must be interested in sending.  The CHANNEL must have room
in its BUFFER for at least one object.

Must be called with *LOCK* held."
  (declare (type operation sending-op))
  (assert (eq 'send (direction sending-op)))
  (let ((buffer (buffer (channel sending-op))))
    (assert (not (jpl-queues:full? buffer)))
    (jpl-queues:enqueue (value sending-op) buffer)))
