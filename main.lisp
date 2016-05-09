(defpackage :rotator
  (:export :main
           :config-dir
           :rules-config-exist?
           :file-size
           :size-from-text
           :size-suffix-to-number
           :rules-config-path)
  (:use :common-lisp :cxml)
  (:import-from :cl-ppcre :scan))

(in-package :rotator)

(defun config-dir ()
  "Возвращает путь конфигурационной директории"
  (pathname
   (concatenate 'string
                (directory-namestring (user-homedir-pathname))
                ".rotator/")))

(defun rules-config-path ()
  "Возвращает путь до xml-конфига, где
   указаны параметры ротации"
  (merge-pathnames (config-dir) #p"config.xml"))

(defun rules-config-exist? ()
  "Проверка на существование файла-конфига"
  (let ((exist (probe-file (rules-config-path))))
    (if exist
        T
        nil)))

(defun file-size (limit value)
  "Предикат осуществляет проверку переданного значения на условие
   соответствия лимиту. Лимит задается в текстовой форме.
   Пример: '<10MB', '>10KB'"
  (let ((expr (scan "(<|>)?\\d+(KB|MB|TB)?" limit))
        (first-sym (char limit 0)))
    (if expr
        (cond
          ((char= #\> first-sym)
           (> value (size-from-text (subseq limit 1))))
          ((char= #\< first-sym)
           (< value (size-from-text (subseq limit 1))))
          (t (= value (size-from-text limit))))
        nil)))

(defun size-from-text (text)
  "Конвертирует размер указанный в текстовом виде
   в числовое представление. Единица измерения - байт"
  (if (= (length text) 0)
      0
      (multiple-value-bind (number pos-inx)
          (parse-integer text :junk-allowed t)
        (* number (size-suffix-to-number (subseq text pos-inx))))))

(defun size-suffix-to-number (suffix)
  "Возвращает множитель единицы измерения,
   основываясь на текстовом представлении"
  (cond ((equal suffix "KB") 1024)
        ((equal suffix "MB") 1048576)
        ((equal suffix "TB") 10737741824)
        (t 1)))

(defun main (argv)
  (print "It's work"))
