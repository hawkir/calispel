(defpackage #:calispel-test
  (:use #:common-lisp #:calispel #:jpl-queues))

(in-package #:calispel-test)

(defun divide-vector (vector count)
  "Returns a vector of COUNT vectors with elements from VECTOR.  The
resulting vectors will be roughly the same length (some may be 1
longer than others), and the sum of the lengths will be equal to the
length of VECTOR.  The order of elements in the returned vectors is
not assured."
  (declare (type vector vector)
	   (type jpl-util:array-dimension count))
  (unless (plusp count)
    (error "COUNT must be greater than zero."))
  (let* ((element-type (array-element-type vector))
	 (shares (make-array count
			     :element-type 'jpl-util:array-dimension
			     :initial-element (floor (length vector) count)))
	 (subvectors (make-array count
				 :element-type `(vector (vector ,element-type)))))
    (dotimes (i (mod (length vector) count))
      (incf (aref shares i)))
    (assert (= (length vector)
	       (reduce #'+ shares :initial-value 0)))
    (dotimes (i count)
      (setf (aref subvectors i)
	    (make-array (aref shares i) :element-type element-type)))
    (loop for vector-start = 0 then vector-end
	  for length across shares
	  for vector-end = (+ vector-start length)
	  for subvector across subvectors
	  doing (replace subvector vector
			 :start2 vector-start :end2 vector-end))
    subvectors))

(defun control-channel-vector (count)
  "Returns a vector of COUNT channels suitable for controlling
threads."
  (loop with out = (make-array count :element-type 'channel)
	for i below count
	for buffer = (make-instance 'jpl-queues:unbounded-fifo-queue)
	for channel = (make-instance 'channel :buffer buffer)
	doing (setf (aref out i) channel)
	finally (return out)))

;;; 1000000 messages is enough to keep my 32-bit 2.4GHz dual Xeon with
;;; SBCL 1.0.18 busy for almost 2 minutes on unbuffered channels.
;;; (Note that there are artificial slowdowns in this test function.)
;;; We need at least ~10 seconds of runtime, and that includes on
;;; faster multicore machines of the future.
(defun test-channel (channel &key (message-count 2000000)
		     (reader-count 4) (writer-count 4)
		     (verbose-p nil))
  "Tests CHANNEL by writing MESSAGE-COUNT messages to it.  The CHANNEL
must not have any buffered messages at the time this function is
called, and no other thread may operate on CHANNEL for the duration of
this function call.  The buffer of CHANNEL, if any, must be exact (it
must not drop messages).

WRITER-COUNT writer threads are created to write to the channel.

READER-COUNT reader threads are created to read from the channel.

The CHANNEL is tested under reader-contention, writer-contention, and
natural conditions.  Under the reader-contention condition, writer
threads will intentionally become slow in order to induce multiple
readers contending for CHANNEL; the buffer of CHANNEL (if any) should
become empty.  Under the writer-contention condition, reader threads
will intentionally become slow in order to induce multiple writer
threads contending for CHANNEL; the buffer of CHANNEL (if any) should
become full (or grow indefinately if unbounded).  Under the natural
condition, no thread will intentionally slow down to induce contention
on the other side.  The condition which is in effect cycles once every
3 seconds; therefore, a high enough MESSAGE-COUNT should be given so
that this function takes at least ~10 seconds to run."
  (declare (type jpl-util:array-dimension
		 message-count reader-count writer-count))
  ;; FIXME: Figure out how to test FIFO order of FIFO-buffered (or
  ;; unbuffered) channels.  Probably involves precise timestamps.
  (let* (;; MESSAGES is the set of messages that will be written to
	 ;; the channel.  Specifically, it is the set of integers
	 ;; [0,MESSAGE-COUNT).  We store the messages in vectors for
	 ;; convenience, but note that putting anything other than the
	 ;; aforementioned set of integers into MESSAGES will result
	 ;; in trouble during verification.
	 (messages (loop with ms = (make-array message-count
					       :element-type 'fixnum)
			 for i below (length ms)
			 doing (setf (aref ms i) i)
			 finally (return ms)))
	 ;; Each writer thread gets a vector of messages that it must
	 ;; write to CHANNEL (in any order).  When the vector is
	 ;; exhausted, the thread terminates.
	 (writer-message-vectors (divide-vector messages writer-count))
	 ;; Per-thread control channels used to induce slowdown or (in
	 ;; the case of reader threads) request termination.
	 (reader-controls (control-channel-vector reader-count))
	 (writer-controls (control-channel-vector writer-count))
	 ;; Each reader thread manages its own vector of elements that
	 ;; it has read from the channel.  When it receives the
	 ;; :CLEAN-UP message on READER-CONTROL, it enters the
	 ;; clean-up state.  In that state, if it cannot read from
	 ;; CHANNEL without blocking, it writes its result vector to
	 ;; READER-RESULTS and terminates.
	 (reader-results (make-instance
			  'channel
			  :buffer (make-instance
				   'jpl-queues:unbounded-random-queue)))
	 ;; Just before any thread terminates, it writes a message
	 ;; with its thread object to this channel.  The main thread
	 ;; waits until all threads have written their expiration
	 ;; notice to this channel.
	 (thread-expiration
	  (make-instance 'channel
			 :buffer (make-instance 'jpl-queues:bounded-fifo-queue
						:capacity (+ reader-count
							     writer-count))))
	 ;; These track the currently-alive threads.  The order of the
	 ;; threads within these vectors is not significant.
	 (readers (make-array reader-count :fill-pointer 0))
	 (writers (make-array writer-count :fill-pointer 0))
	 ;; The current contention condition.
	 (condition nil)
	 ;; The next time (in terms of
	 ;; JPL-UTIL:GET-REASONABLE-REAL-TIME) that the contention
	 ;; condition should be cycled.
	 (condition-cycle-time 0)
	 ;; The start and end times of the threads (in terms of
	 ;; JPL-UTIL:GET-REASONABLE-REAL-TIME).
	 start-time end-time)
    (flet ((reader (control)
	     (let ((out (make-array 1024 :element-type 'fixnum
				    :adjustable t :fill-pointer 0))
		   (cleanup? nil)
		   (slow? nil))
	       (loop
		 (when slow?
		   (sleep 1/100))
		 (pri-alt ((? control msg)
			   (ecase msg
			     (:clean-up (setf cleanup? t))
			     (:high-speed (setf slow? nil))
			     (:low-speed (setf slow? t))))
			  ((? channel msg)
			   (declare (type fixnum msg))
			   (vector-push-extend msg out))
			  ((otherwise :timeout (if cleanup? 0 nil))
			   (! reader-results out)
			   (! thread-expiration (bt:current-thread))
			   (return))))))
	   (writer (control messages-v)
	     (declare (type (vector fixnum) messages-v))
	     (let ((i 0)
		   (slow? nil))
	       (declare (type fixnum i))
	       (loop
		 (when slow?
		   (sleep 1/100))
		 (pri-alt ((? control msg)
			   (ecase msg
			     (:high-speed (setf slow? nil))
			     (:low-speed (setf slow? t))))
			  ((! channel (aref messages-v i))
			   (incf i)
			   (when (= i (length messages-v))
			     (! thread-expiration (bt:current-thread))
			     (return)))))))
	   (safe-vector-push (new-element vector)
	     (unless (vector-push new-element vector)
	       (error "Ran out of room for new element."))))
      (when verbose-p
	(format t "~&Starting threads.~&"))
      (setf start-time (jpl-util:get-reasonable-real-time))
      (loop for i from 1
	    for name = (format nil "Calispel-test-reader-~D" i)
	    for control across reader-controls
	    doing (safe-vector-push (bt:make-thread (jpl-util:curry-left
						     #'reader control)
						    :name name)
				    readers))
      (loop for i from 1
	    for name = (format nil "Calispel-test-writer-~D" i)
	    for messages-v across writer-message-vectors
	    for control across writer-controls
	    doing (safe-vector-push (bt:make-thread (jpl-util:curry-left
						     #'writer
						     control messages-v)
						    :name name)
				    writers))
      (labels ((cycle-condition ()
		 (unless (< (jpl-util:get-reasonable-real-time)
			    condition-cycle-time)
		   (setf condition (ecase condition
				     ((nil) :none)
				     (:none :reader)
				     (:reader :writer)
				     (:writer :none)))
		   (setf condition-cycle-time
			 (+ 3 (jpl-util:get-reasonable-real-time)))
		   (flet ((tell-all (control-v msg)
			    (loop for control across control-v
				  doing (! control msg))))
		     (ecase condition
		       (:none (tell-all reader-controls :high-speed)
			      (tell-all writer-controls :high-speed))
		       (:reader (tell-all reader-controls :high-speed)
				(tell-all writer-controls :low-speed))
		       (:writer (tell-all reader-controls :low-speed)
				(tell-all writer-controls :high-speed))))))
	       (time-until-next-cycle ()
		 (max 0 (- condition-cycle-time
			   (jpl-util:get-reasonable-real-time))))
	       (wait-until-threads-terminate (thread-type-label thread-vector)
		 (loop
		   (cycle-condition)
		   (when (zerop (length thread-vector))
		     (return))
		   (pri-alt ((? thread-expiration thread)
			     (unless (vector-pop-element thread-vector
							 thread)
			       (error "While awaiting the termination of ~
                                       ~A threads, got an unexpected ~
                                       thread: ~S"
				      thread-type-label thread))
			     (when verbose-p
			       (format t "~&Reaped ~A thread: ~S~&"
				       thread-type-label thread)))
			    ((otherwise :timeout (time-until-next-cycle))))))
	       (vector-pop-element (vector element)
		 ;; Returns true when ELEMENT was in VECTOR.
		 (let ((pos (position element vector)))
		   (unless (null pos)
		     (jpl-util:vector-delete vector pos))
		   (not (null pos)))))
	(cycle-condition)
	(wait-until-threads-terminate "writer" writers)
	;; Ask all readers to terminate when they get the chance.
	(loop for control across reader-controls
	      doing (! control :clean-up))
	(wait-until-threads-terminate "reader" readers)
	(setf end-time (jpl-util:get-reasonable-real-time))
	(when verbose-p
	  (format t "~&All threads finished.~&"))
	;; Collect and verify results.
	(let ((seen-messages (make-array message-count :element-type 'bit
					 :initial-element 0))
	      (duplicate-count 0)
	      (drop-count 0)
	      (result-vector-count 0))
	  (loop
	    (pri-alt ((? reader-results result-v)
		      (incf result-vector-count)
		      (loop for message across result-v
			    unless (zerop (aref seen-messages message))
			    doing (incf duplicate-count)
			    doing (setf (aref seen-messages message) 1)))
		     (otherwise (return))))
	  (assert (= reader-count result-vector-count))
	  (loop for message from 0
		for seen-bit across seen-messages
		when (zerop seen-bit)
		doing (incf drop-count))
	  (unless (and (zerop duplicate-count)
		       (zerop drop-count))
	    (error "~D duplicated message~:P and ~D dropped message~:P."
		   duplicate-count drop-count)))
	;; Verify no buffered messages in CHANNEL and other channels.
	(pri-alt ((? channel)
		  (error "Lingering messages on CHANNEL."))
		 ((? reader-results)
		  (error "Lingering messages on READER-RESULTS."))
		 ((? thread-expiration)
		  (error "Lingering messages on THREAD-EXPIRATION."))
		 ((otherwise :timeout 3)
		  (values)))
	(flet ((verify-control (label chan)
		 (loop
		   (pri-alt ((? chan msg)
			     (case msg
			       ((:low-speed :high-speed)) ; No-op.
			       (otherwise
				(error "Lingering non-speed messages on ~A ~
                                        control channel."
				       label))))
			    (otherwise (return))))))
	  (loop for control across reader-controls
		doing (verify-control "reader" control))
	  (loop for control across writer-controls
		doing (verify-control "writer" control)))
	(when verbose-p
	  (let* ((time (- end-time start-time))
		 (rate (/ message-count time)))
	    (format t "~&Succeeded: ~,2F seconds run-time, ~
                       ~,2F messages/second.~&"
		    time rate)))
	(values)))))

;;; These parameters keep my dual Xeon busy for about 12 minutes.
(defun test-concurrency (&rest kw-args &key
 			 ;; Enumerate the allowable keyword args, even
 			 ;; the ones used only by TEST-CHANNEL, to
 			 ;; prevent accepting VERBOSE-P.  We can't let
 			 ;; multiple threads print concurrently.
			 (channel-count 8)
			 (make-channel-fn (lambda () (make-instance 'channel)))
			 (message-count 2000000)
			 (reader-count 4) (writer-count 4))
  "Tests concurrency by creating CHANNEL-COUNT CHANNELs and running
TEST-CHANNEL against each, simultaneously.

MAKE-CHANNEL-FN is a designator of a function of no arguments that
returns a fresh CHANNEL.  It is used to produce the test channels.

MESSAGE-COUNT, READER-COUNT, and WRITER-COUNT are as in TEST-CHANNEL;
note that these values are per-channel-test, not globally."
  (declare (ignore message-count reader-count writer-count))
  (let ((futures (loop repeat channel-count
		       collecting (let ((channel (funcall make-channel-fn)))
				    (eager-future2:pexec
				      (apply #'test-channel channel
					     :allow-other-keys t kw-args))))))
    ;; YIELD them to block until all the TEST-CHANNEL calls return,
    ;; and to check for caught errors.
    (dolist (future futures)
      (eager-future2:yield future))))
