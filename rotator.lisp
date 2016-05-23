(defpackage :rotator.rotator
  (:export :remover
           :rotate)
  (:use :common-lisp :cl-fad))

(in-package :rotator.rotator)


(defclass rotator ()
  ((params
    :initarg :params
    :initform '())))

(defgeneric rotate (rotator logger)
  (:documentation "Здесь происходит ротация файла,
   указанного в path"))


(defclass remover (rotator)
  ())

(defmethod rotate ((self rotator) logger)
  (print "It's work!"))
