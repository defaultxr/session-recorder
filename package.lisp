;;;; package.lisp

(defpackage #:swank-recorder
  (:use #:cl)
  (:export
   #:record-string
   #:start-recording
   #:stop-recording))
