(in-package :boten-anna)

(defvar *conns* ())
(defparameter *conf* ())

(defmacro defhook (conn event &body body)
  (let ((irc-class (intern (concatenate 'string "IRC-" (string event) "-MESSAGE"))))
    `(add-hook ,conn (quote ,irc-class) (lambda (message) ,@body t))))

(defmacro -> (f &rest fs)
  (if fs
    (append f `((-> ,@fs)))
    f))

(defun compare-server-connection (connection server)
  (let ((s1 (server-name connection))
        (s2 (getf server :server))
        (p1 (server-port connection))
        (p2 (getf server :port)))
    (if (string= s1 s2)
        (if (= p1 p2)
            0
            (if (< p1 p2)
                -1
                1))
        (if (string< s1 s2)
            -1
            1))))

(define-condition duplicate-server-entry-error (error)
  ((text :initarg :text :reader text)))

(defun compare-server-entry (e1 e2)
  "Compares two server configuration entries alphabetically. If the two are equal, signals an error."
  (let ((s1 (getf e1 :server))
        (s2 (getf e2 :server))
        (p1 (getf e1 :port))
        (p2 (getf e2 :port)))
    (if (string= s1 s2)
        (if (= p1 p2)
            (error 'duplicate-server-entry-error :text (format nil "~a ~a" s1 p1))
            (< p1 p2))
        (string< s1 s2))))

(defun join-channels (connection channels)
  (let ((pm (getf *conf* :part-message)))
    (maphash #'(lambda (chan obj)
  	       (declare (ignore obj))
  	       (if (find-if #'(lambda (x) (string= x chan)) channels)
  		   nil
  		   (part connection chan (if pm pm (getf *conf* :quit-message)))))
  	   (channels connection)))
  (loop for c in channels do
       (join connection c)))

(defun create-connection (server-entry)
  (let ((c (connect
            :nickname (getf *conf* :nickname)
            :server (getf server-entry :server)
            :port (getf server-entry :port)
            :connection-security (if (getf server-entry :ssl) :ssl :none)
            :logging-stream (getf *conf* :debug-stream)))
        (chans (getf server-entry :channels)))
    (defhook c rpl_endofmotd (join-channels c chans))
    (defhook c privmsg (reload-conf))
    (setf (server-port c) (getf server-entry :port))
    (start-background-message-handler c)
    c))

(defun update-connections (connections servers)
  (if (null servers)
      (loop for c in connections do
           (quit c (getf *conf* :quit-message)))
      (if (null connections)
          (loop for s in servers do
               (push (create-connection s) *conns*))
          (case (compare-server-connection (car connections) (car servers))
            (-1 (progn
                  (quit (car connections) (getf *conf* :quit-message))
                  (update-connections (cdr connections) servers)))
            (0 (progn
	         (nick (car connections) (getf *conf* :nickname))
                 (join-channels (car connections) (getf (car servers) :channels)))
                 (push (car connections) *conns*)
                 (update-connections (cdr connections) (cdr servers)))
            (1 (progn
                 (push (create-connection (car servers)) *conns*)
                 (update-connections connections (cdr servers))))))))

(defun reload-conf ()
  "Reads the configuration from conf.lisp and updates *conns* and *conn* to reflect it."
  (let ((conf (read (open "conf.lisp"))))
    ; check structure and integrity
    (setf *conf* conf))
  (let ((servers (sort (getf *conf* :servers) #'compare-server-entry))
        (connections (reverse *conns*)))
    (setf (getf *conf* :servers) servers)
    (setf *conns* nil)
    (update-connections connections servers)))

(defun boten-anna ()
  (reload-conf))

(boten-anna)
