(in-package :cl-user)
(defpackage rotator-test
  (:use :cl
        :rotator
        :prove))
(in-package :rotator-test)

;; NOTE: To run this test file, execute `(asdf:test-system :rotator)' in your Lisp.

(plan 1)

(ok (rotator.condition:always-true "" 0))

(finalize)
