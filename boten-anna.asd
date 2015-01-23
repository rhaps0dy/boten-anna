;;;; $Id: boten-anna.asd 228 2012-09-23 16:20:09Z ehuelsmann $
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:boten-anna-system
    (:use #:cl #:asdf))

(in-package #:boten-anna-system)

(defsystem boten-anna
    :name "boten-anna"
    :author "Adrià Garriga-Alonso"
    :version "0.0"
    :licence "MIT"
    :description "Yet Annaother IRC Boten."
    :depends-on (:cl-irc :cl+ssl)
    :properties ((#:author-email . "adria@monkingme.com"))
    :components ((:file "package")
                 (:file "main")))
