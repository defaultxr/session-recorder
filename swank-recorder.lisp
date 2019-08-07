;;;; swank-recorder.lisp

(in-package #:swank-recorder)

(defvar *saved-swank-listener-eval* #'swank-repl:listener-eval)

(defvar *saved-swank-compile-string-for-emacs* #'swank:compile-string-for-emacs)

(defvar *output-file* nil)

(defun write-metadata ()
  "Write some basic metadata about the session."
  (print (list :internal-time-units-per-second internal-time-units-per-second) *output-file*)
  (fresh-line *output-file*)
  (force-output *output-file*))

(defun record-string (string)
  "Record a string of Lisp code that was sent to Swank."
  (format *output-file* "(~d ~a)~%" (get-internal-real-time) string)
  (force-output *output-file*))

(defun listener-eval (&rest args)
  "Internal swank-recorder function that is swapped with `swank-repl:listener-eval' when recording is started."
  (record-string (string-right-trim (list #\newline #\space) (elt args 0)))
  (apply *saved-swank-listener-eval* args))

(defun compile-string-for-emacs (&rest args)
  "Internal swank-recorder function that is swapped with `swank:compile-string-for-emacs' when recording is started."
  (record-string (elt args 0))
  (apply *saved-swank-compile-string-for-emacs* args))

(defun start-recording (path)
  "Start recording Swank inputs to PATH."
  (if *output-file*
      (warn "swank-recorder is already recording; ignoring...")
      (progn
        (setf *output-file* (open path :direction :output :if-exists :rename-and-delete))
        (write-metadata)
        (setf (fdefinition 'swank-repl:listener-eval) (fdefinition 'listener-eval)
              (fdefinition 'swank:compile-string-for-emacs) (fdefinition 'compile-string-for-emacs)))))

(defun stop-recording ()
  "Stop recording Swank inputs. Strictly speaking, this is not necessary, because "
  (close *output-file*)
  (setf *output-file* nil
        (fdefinition 'swank-repl:listener-eval) *saved-swank-listener-eval*
        (fdefinition 'swank:compile-string-for-emacs) *saved-swank-compile-string-for-emacs*))
