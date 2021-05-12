;;;; package.lisp

(defpackage #:session-recorder
  (:use #:cl
        #:alexandria)
  (:export
   #:record-string
   #:start-recording
   #:stop-recording

   #:*player*
   #:playback-start
   #:playback-next
   #:playback-stop))
