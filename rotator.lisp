(defpackage :rotator.rotator
  (:export :remover
           :ident
           :info
           :params
           :rotate)
  (:import-from :cl-log
                :log-message)
  (:import-from :cl-fad
                :file-exists-p)
  (:use :common-lisp))

(in-package :rotator.rotator)


(defclass rotator ()
  ((ident
    :reader ident)
   (params
    :initarg :params
    :accessor params
    :initform '())))

(defgeneric rotate (rotator path)
  (:documentation "Здесь происходит ротация файла,
   указанного в path"))


(defun log-label (rotator)
  "Возвращает 'подпись' ротатора для представления в
   лог-файле"
  (format nil "[ROTATOR] (~a)" (ident rotator) ))

(defclass remover (rotator)
  ())

(defmethod initialize-instance :after ((self remover) &key)
  (setf (slot-value self 'ident) :remover))

(defmethod rotate ((self remover) path)
  (progn
    (delete-file (pathname path))
    (log-message :info
                 (format nil "~a Файл ~s успешно удален" (log-label self) path))))

(defclass info (rotator)
  ())

(defmethod initialize-instance :after ((self info) &key)
  (setf (slot-value self 'ident) :info))

(defmethod rotate ((self info) path)
    (log-message :info
                 (format nil "~a Файл ~s был подвергнут ротации" (log-label self) path)))
