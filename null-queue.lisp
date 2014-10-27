(in-package #:calispel)

(defclass null-queue (jpl-queues:queue)
  ()
  (:documentation "The null queue.  Used for unbuffered CHANNELs.

Think of it as the NULL class, but for queues."))

(defmethod jpl-queues:empty? ((queue null-queue))
  t)

(defmethod jpl-queues:full? ((queue null-queue))
  t)

(defmethod jpl-queues:size ((queue null-queue))
  0)

(defmethod jpl-queues:capacity ((queue null-queue))
  0)

(defmethod jpl-queues:enqueue (object (queue null-queue))
  (error "It is an error to ENQUEUE to a NULL-QUEUE."))

(defmethod jpl-queues:dequeue ((queue null-queue))
  (error "It is an error to DEQUEUE from a NULL-QUEUE."))

(defmethod jpl-queues:dequeue-object-if (predicate (queue null-queue) &key &allow-other-keys)
  ;; We can guarantee that no matching OBJECT is in this QUEUE.
  (values))

;;; Since NULL-QUEUE has no state, we can keep a single instance.
;;; Don't think of it as the queue analog to NIL (of lists), because
;;; (EQ +NULL-QUEUE+ (MAKE-INSTANCE 'NULL-QUEUE)) is false.

(defparameter +null-queue+ (make-instance 'null-queue))
