(defpackage rotator.utils
  (:use :common-lisp
        :xpath)
  (:export :xpath-attr-val
           :pretty-universal-time
           :re-begin-and-end-str))

(in-package :rotator.utils)


(defun xpath-attr-val (attr-name node)
  "Возвращает значение атрибута указанного xml узла"
  (xpath:string-value
   (xpath:evaluate
    (concatenate 'string "@" attr-name)
    node)))

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
