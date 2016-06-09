(defpackage :rotator.condition
  (:export :file-size-more
           :file-name-match
           :always-true
           :always-false
           :file-size-less)
  (:use :common-lisp)
  (:import-from :cl-fad :file-exists-p)
  (:import-from :cl-ppcre
                :regex-replace-all
                :scan-to-strings
                :scan)
  (:import-from :rotator.utils
                :re-begin-and-end-str))

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

(defun name-pattern-to-re (expr)
  (re-begin-and-end-str
   (regex-replace-all "\\*"
                      (regex-replace-all "\\." expr "\\.")
                      ".*")))

(defun parse-duration-value (duration suffix)
  (let*  ((reg-exp (format nil "[^\\d](\\d{1,3})~a" suffix))
          (result (multiple-value-list
                   (scan-to-strings reg-exp duration))))
    (if (not (null (first result)))
        (parse-integer (elt (second result) 0))
        nil)))

(defun duration-to-seconds (duration)
  "Функция парсит указанный в строке временной промежуток и возвращает
   кол-во секунд в нем. Пример промежутка '10s 1m 60h 7D 13M 1Y'.
   s - секунды, m - минуты, h - часы, D - дни, M - месяцы, Y - года"
  nil)

(defmacro defcondition (name params &body form)
  `(defun ,name ,params
     (if (file-exists-p ,(first params))
         ,@form
         nil)))

;; Файл больше указанного размера?
(defcondition file-size-more (path limit)
  (let ((limit-value (size-from-text limit)))
    (if (and
         limit-value
         (scan "\\d+(KB|MB|GB|B)?" limit))
        (> (file-size (pathname path)) limit-value)
        nil)))

;; Файл меньше указанного размера?
(defcondition file-size-less (path limit)
  (let ((limit-value (size-from-text limit)))
    (if (and
         limit-value
         (scan "\\d+(KB|MB|GB|B)?" limit))
        (< (file-size (pathname path)) limit-value)
        nil)))

;; Имя файла соответствует указанному шаблону?
(defcondition file-name-match (path pattern)
  (if (scan (name-pattern-to-re pattern) (file-namestring path))
      t
      nil))

;; Условие всегда возвращает истину
(defcondition always-true (path limit)
  t)

;; Условие всегда возвращает ложь
(defcondition always-false (path limit)
  nil)
