#!/usr/bin/sbcl --script
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload 'cl-irc)
(ql:quickload 'cl+ssl)

(asdf:operate 'asdf:load-op 'boten-anna)
