#|
  This file is a part of rotator project.
  Copyright (c) 2017 Ito Dimercel (xolcman@gmail.com)
|#

#|
  Author: Ito Dimercel (xolcman@gmail.com)
|#

(in-package :cl-user)
(defpackage rotator-asd
  (:use :cl :asdf))
(in-package :rotator-asd)

(defsystem rotator
  :version "0.1"
  :author "Ito Dimercel"
  :license "LLGPL"
  :depends-on (:cxml
               :xpath
               :cl-ppcre
               :cl-fad
               :rutils
               :local-time
               :cl-rules
               :cl-log)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "config")
                 (:file "condition")
                 (:file "rotator")
                 (:file "main"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op rotator-test))))
