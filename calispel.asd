;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem "calispel"
  :version "0.1"
  :maintainer "J.P. Larocque"
  :author "J.P. Larocque, et al. (see COPYRIGHT.txt)"
  :licence "ISC-style and other permissive (see COPYRIGHT.txt)"
  :description "Thread-safe message-passing channels, in the style of
the occam programming language."
  :components (;; Core data structures and algorithms.
	       (:file "core"
                :depends-on ("null-queue"
			     "package"))
	       ;; Basic operations: ? and !
	       (:file "basic"
		:depends-on ("core"
			     "package"))
	       ;; The *-ALT macros.
	       (:file "alt"
		:depends-on ("core"
			     "package"))
	       ;; The null queue.
	       (:file "null-queue"
                :depends-on ("package"))
	       ;; Testing.
	       (:file "test"
		:depends-on ("core"
			     "basic"
			     "alt"
			     "package"))
	       ;; Package definition.
               (:file "package"))
  :depends-on ("jpl-queues"
	       "eager-future"
	       "bordeaux-threads"
	       (:version "jpl-util" "0.2")))
