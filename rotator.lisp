(defpackage :rotator.rotator
  (:export :remover
           :rotate)
  (:use :common-lisp :cl-fad))

(in-package :rotator.rotator)


(defclass rotator ()
  ((log-path
    :initarg :log-path
    :initform "~/.rotator/rotate.log"
    :reader log-path)
   (params
    :initarg :params
    :initform '())))

(defgeneric rotate (rotator path)
  (:documentation "Здесь происходит ротация файла,
   указанного в path"))


(defclass remover (rotator)
  ())

(defmethod rotate ((self rotator) path)
  (print "It's work!"))
