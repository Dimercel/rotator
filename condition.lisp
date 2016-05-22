(defpackage :rotator.condition
  (:export :file-size-more
           :file-size-less)
  (:use :common-lisp)
  (:import-from :cl-fad :file-exists-p)
  (:import-from :cl-ppcre :scan))

(in-package :rotator.condition)

(defun file-size (path)
  "Возвращает размер файла в байтах."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (file-length in)))

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
        ((equal suffix "GB") 10737741824)
        (t 1)))

(defmacro defcondition (name params &body form)
  `(defun ,name ,params
     (if (file-exists-p ,(first params))
         ,@form
         nil)))

(defcondition file-size-more (path limit)
  (let ((limit-value (size-from-text limit)))
    (if (and
         limit-value
         (scan "\\d+(KB|MB|GB|B)?" limit))
        (> (file-size (pathname path)) limit-value)
        nil)))

(defcondition file-size-less (path limit)
  (let ((limit-value (size-from-text limit)))
    (if (and
         limit-value
         (scan "\\d+(KB|MB|GB|B)?" limit))
        (< (file-size (pathname path)) limit-value)
        nil)))
