;;;; swank-recorder.asd

(asdf:defsystem #:swank-recorder
  :description "Simple listener recorder for Swank."
  :author "modula t. <defaultxr at gmail dot com>"
  :license "MIT"
  :version "0.5"
  :serial t
  :depends-on (#:swank)
  :components ((:file "package")
               (:file "swank-recorder")))
