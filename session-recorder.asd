;;;; session-recorder.asd

(asdf:defsystem #:session-recorder
  :name "session-recorder"
  :description "Simple listener recorder for Swank or Slynk"
  :author "modula t."
  :license "MIT"
  :version "0.7"
  :homepage "https://github.com/defaultxr/session-recorder"
  :bug-tracker "https://github.com/defaultxr/session-recorder/issues"
  :mailto "defaultxr at gmail dot com"
  :source-control (:git "git@github.com:defaultxr/session-recorder.git")
  :serial t
  :depends-on (#+swank #:swank
               #+slynk #:slynk
               #:alexandria
               #:bordeaux-threads)
  :components ((:file "package")
               (:file "session-recorder")))
