(defpackage rotator.utils
  (:export :pretty-universal-time
           :re-begin-and-end-str)
  (:use :cl))

(in-package :rotator.utils)


(defun re-begin-and-end-str (reg-exp)
  "Добавляет спецификаторы начала и конца
   строки в рег. выражение"
  (concatenate 'string "^" reg-exp "$"))

(defun pretty-universal-time (time)
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~2,'0d.~2,'0d.~d ~2,'0d:~2,'0d:~2,'0d"
            date
            month
            year
            hour
            minute
            second)))
