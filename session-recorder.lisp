;;;; session-recorder.lisp
;; FIX: does not record code evaluated with slime comma commands
;; FIX: investigate CL's built-in `dribble' function...

(in-package #:session-recorder)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +listener-eval-function+
    #+swank 'swank-repl:listener-eval
    #+slynk 'slynk-mrepl::mrepl-eval-1)

  (defconstant +interactive-eval-function+
    #+swank 'swank:interactive-eval
    #+slynk 'slynk:interactive-eval)

  (defconstant +compile-string-for-emacs-function+ ;; `slynk:eval-for-emacs' ? or `slynk-backend::slynk-compile-string' ?
    #+swank 'swank:compile-string-for-emacs
    #+slynk 'slynk:compile-string-for-emacs))

(defvar *saved-listener-eval* (fdefinition '#.+listener-eval-function+))

(defvar *saved-interactive-eval* (fdefinition '#.+interactive-eval-function+))

(defvar *saved-compile-string-for-emacs* (fdefinition '#.+compile-string-for-emacs-function+))

;;; recording

(defvar *output-file* nil)

(defun write-to-log (item)
  (prin1 item *output-file*)
  (fresh-line *output-file*)
  (force-output *output-file*))

(defun write-metadata ()
  "Write some basic metadata about the session."
  (write-to-log (list :internal-time-units-per-second internal-time-units-per-second))
  (write-to-log (list :random-state *random-state*)))

(defun record-string (string)
  "Record a string of Lisp code that was sent to Lisp."
  (write-to-log (list (get-internal-real-time) (string-trim (list #\newline #\space) string))))

(defun listener-eval (&rest args)
  "Internal session-recorder function that is swapped with `listener-eval' when recording is started."
  (record-string
   #+swank (elt args 0)
   #+slynk (elt args 1))
  (apply *saved-listener-eval* args))

(defun interactive-eval (&rest args)
  "Internal session-recorder function that is swapped with `interactive-eval' when recording is started."
  (record-string (elt args 0))
  (apply *saved-interactive-eval* args))

(defun compile-string-for-emacs (&rest args)
  "Internal session-recorder function that is swapped with `compile-string-for-emacs' when recording is started."
  (record-string (elt args 0))
  (apply *saved-compile-string-for-emacs* args))

(defun start-recording (path)
  "Start recording Lisp inputs to PATH."
  (if *output-file*
      (warn "session-recorder is already recording; ignoring...")
      (progn
        (setf *output-file* (open path :direction :output :if-exists :rename-and-delete))
        (write-metadata)
        (setf (fdefinition '#.+listener-eval-function+) (fdefinition 'listener-eval)
              (fdefinition '#.+interactive-eval-function+) (fdefinition 'interactive-eval)
              (fdefinition '#.+compile-string-for-emacs-function+) (fdefinition 'compile-string-for-emacs)))))

(defun stop-recording ()
  "Stop recording inputs, close the log file, and restore all swapped functions. Strictly speaking, it is not necessary to call this function to write the log, because all events recorded are flushed to it immediately."
  (close *output-file*)
  (setf *output-file* nil
        (fdefinition '#.+listener-eval-function+) *saved-listener-eval*
        (fdefinition '#.+interactive-eval-function+) *saved-interactive-eval*
        (fdefinition '#.+compile-string-for-emacs-function+) *saved-compile-string-for-emacs*))

;;; playback

(defvar *player* nil
  "The player that is currently playing back the recording.")

(defstruct player
  last ;; last index played, so we can easily find the next one
  file
  metadata
  recording
  thread
  print-p)

;; FIX: doesn't seem to handle reader variables like *, **, and ***...

(defmethod print-object ((player player) stream)
  (print-unreadable-object (player stream :type 'player)
    (with-slots (file thread print-p) player
      (format stream ":FILE ~s :THREAD ~s :PRINT-P ~s" file thread print-p))))

(defun playback-start (file &key (from :first) (print-p t))
  "Start playing back the recording in FILE, optionally from a specific point. If FROM is a number, start playing from that time in the recording. FROM can also be :first (the default), in which case playback is started from the first recorded event in the recording.

See also: `playback-next', `playback-stop'."
  (when *player*
    (playback-stop))
  (destructuring-bind (metadata recording)
      (with-open-file (rec file :direction :input)
        (loop :for sexp = (read rec nil 'eof)
              :until (eql sexp 'eof)
              :if (and sexp (integerp (car sexp)))
                :collect sexp :into recording
              :if (and sexp (symbolp (car sexp)))
                :collect sexp :into metadata
              :finally (return (list metadata recording))))
    (setf *player* (make-player :next from
                                :print-p print-p
                                :file file
                                :metadata metadata
                                :recording (coerce recording 'vector))
          ;; (player-thread *player*) (bt:make-thread
          ;;                           (lambda ()
          ;;                             (let ((player *player*))
          ;;                               (labels ((play (item)
          ;;                                          (let ((code (cadr item)))
          ;;                                            (when (player-print-p player)
          ;;                                              (format t "Playing: ~a~%" code))
          ;;                                            (eval (read-from-string code))
          ;;                                            )))
          ;;                                 (with-open-file (rec (player-file player) :direction :input)
          ;;                                   (loop
          ;;                                      (when-let ((res (read rec nil nil))
          ;;                                                 (next (player-next player)))
          ;;                                        (etypecase (car res)
          ;;                                          (symbol (format t "Diagnostic: ~s~%" res))
          ;;                                          (number (etypecase next
          ;;                                                    (symbol
          ;;                                                     (case next
          ;;                                                       (:stop
          ;;                                                        (return-from nil nil))
          ;;                                                       (:first
          ;;                                                        (play res))))
          ;;                                                    (number
          ;;                                                     (when (>= (car res) next)
          ;;                                                       (play res)))))))
          ;;                                      (sleep (/ 1 internal-time-units-per-second)))))))
          ;;                           :name "session-recorder playback thread")
          )))

(defun playback-next (next)
  (if (< next (player-next *player*))
      (progn
        (playback-stop)
        (playback-start (player-recording *player*) :from (player-next *player*) :print-p (player-print-p *player*)))
      (setf (player-next *player*) next)))

(defun playback-stop ()
  (setf (player-next *player*) :stop))

(defun player-elt (player index)
  "Get the nth item in PLAYER's recording.

See also: `player-elt-after', `player-index-after'"
  (elt (player-recording player) index))

(defun player-index-after (player time)
  "Get the index of the next item after TIME in PLAYER's recording. If TIME is a float, consider the time in seconds instead of internal time units.

See also: `player-elt-after', `player-elt'"
  (let ((time (etypecase time
                (integer time)
                (float (if-let ((itups (assoc :internal-time-units-per-second (player-metadata *player*))))
                         (* time (cadr itups))
                         (error "internal-time-units-per-second was not found in the recording metadata.") ;; FIX: allow the user to specify one?
                         )))))
    (position-if (lambda (x) (> (car x) time)) (player-recording player))))

(defun player-elt-after (player time)
  "Get the next item after TIME in PLAYER's recording. If TIME is a float, consider the time in seconds instead of internal time units.

See also: `player-elt-after', `player-elt'"
  (player-elt player (player-index-after player time)))

