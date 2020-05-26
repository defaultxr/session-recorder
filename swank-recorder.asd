;;;; swank-recorder.asd

(asdf:defsystem #:swank-recorder
  :name "swank-recorder"
  :description "Simple listener recorder for Swank"
  :author "modula t."
  :license "MIT"
  :version "0.5"
  :homepage "https://github.com/defaultxr/swank-recorder"
  :bug-tracker "https://github.com/defaultxr/swank-recorder/issues"
  :mailto "defaultxr at gmail dot com"
  :source-control (:git "git@github.com:defaultxr/swank-recorder.git")
  :serial t
  :depends-on (#:swank)
  :components ((:file "package")
               (:file "swank-recorder")))
