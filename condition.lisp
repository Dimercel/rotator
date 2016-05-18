(defpackage :rotator.condition
  (:export :file-size-limit)
  (:use :common-lisp :cl-fad)
  (:import-from :cl-fad :file-exists-p)
  (:import-from :cl-ppcre :scan))

(in-package :rotator.condition)

(defun file-size (path)
  "Возвращает размер файла в байтах."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (file-length in)))

(defun file-size-in-limit? (limit value)
  "Предикат осуществляет проверку переданного значения на условие
   соответствия лимиту. Лимит задается в текстовой форме.
   Пример: '<10MB', '>10KB'"
  (let ((expr (scan "(<|>)?\\d+(KB|MB|TB|B)?" limit))
        (first-sym (char limit 0)))
    (if expr
        (cond
          ((char= #\> first-sym)
           (> value (size-from-text (subseq limit 1))))
          ((char= #\< first-sym)
           (< value (size-from-text (subseq limit 1))))
          (t (= value (size-from-text limit))))
        nil)))

(defun size-from-text (text &optional (bad-result nil))
  "Конвертирует размер указанный в текстовом виде
   в числовое представление. Единица измерения - байт"
  (if (= (length text) 0)
      0
      (multiple-value-bind (number pos-inx)
          (parse-integer text :junk-allowed t)
        (if (null number)
            bad-result
            (* number (size-suffix-to-number (subseq text pos-inx)))))))

(defun size-suffix-to-number (suffix)
  "Возвращает множитель единицы измерения,
   основываясь на текстовом представлении"
  (cond ((equal suffix "B") 1)
        ((equal suffix "KB") 1024)
        ((equal suffix "MB") 1048576)
        ((equal suffix "TB") 10737741824)
        (t 1)))

(defmacro defcondition (name params &body form)
  `(defun ,name ,params
     (if (file-exists-p ,(first params))
         ,@form
         nil)))

(defcondition file-size-limit (path limit)
  (file-size-in-limit?
   limit
   (file-size (pathname path))))
