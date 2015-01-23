;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :boten-anna
    (:use :common-lisp :cl-irc :cl+ssl)
    (:export boten-anna)))
