(in-package #:common-lisp-user)

(defpackage #:calispel
  (:export ;; Basic functionality.
           #:channel
	   #:?
           #:!
	   #:fair-alt
	   #:pri-alt
	   #:otherwise
	   
	   #:null-queue
	   #:+null-queue+
	   
	   ;; Provided for dynamic alternation.
	   #:operation-alternate
	   #:operation
	   #:direction
	   #:value
	   #:send
	   #:receive
	   
	   ;; Testing.
	   #:test-channel
	   #:test-concurrency)
  (:shadowing-import-from #:jpl-util
			  #:sort #:nsort
			  #:stable-sort #:nstable-sort)
  (:use #:common-lisp))
