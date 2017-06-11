#|
  This file is a part of rotator project.
  Copyright (c) 2017 Ito Dimercel (xolcman@gmail.com)
|#

(in-package :cl-user)
(defpackage rotator-test-asd
  (:use :cl :asdf))
(in-package :rotator-test-asd)

(defsystem rotator-test
  :author "Ito Dimercel"
  :license "LLGPL"
  :depends-on (:rotator
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "rotator"))))
  :description "Test system for rotator"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
