(defpackage rotator.utils
  (:use :common-lisp
        :xpath)
  (:export :xpath-attr-val
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
