(defpackage :rotator.rotator
  (:export :remover
           :rotate)
  (:import-from :cl-log
                :log-message)
  (:import-from :cl-fad
                :file-exists-p)
  (:use :common-lisp))

(in-package :rotator.rotator)


(defclass rotator ()
  ((params
    :initarg :params
    :initform '())))

(defgeneric rotate (rotator path)
  (:documentation "Здесь происходит ротация файла,
   указанного в path"))


(defclass remover (rotator)
  ())

(defmethod rotate ((self rotator) path)
  (if (file-exists-p path)
      (progn
        (delete-file (pathname path))
        (log-message :info "[Rotator] Файл успешно удален."))
      (log-message :warning "[Rotator] Файл не существует!")))
