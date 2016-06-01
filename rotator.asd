(defsystem "rotator"
  :description "Files rotator"
  :version "0.1"
  :author "Ito Dimercel <xolcman@gmail.com>"
  :licence "MIT"
  :depends-on ("cxml"
               "xpath"
               "cl-ppcre"
               "cl-fad"
               "rutils"
               "cl-log")
  :components ((:file "utils")
               (:file "config")
               (:file "condition")
               (:file "rotator")
               (:file "main")))
