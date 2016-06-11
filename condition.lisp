(defpackage :rotator.condition
  (:export :file-size-more
           :file-size-less
           :file-name-match
           :file-name-not-match
           :file-age-greater
           :file-age-less
           :always-true
           :always-false)
  (:use :common-lisp)
  (:import-from :cl-fad :file-exists-p)
  (:import-from :local-time
                :timestamp-
                :timestamp<
                :universal-to-timestamp
                :now)
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

(defun parse-duration-value (duration suffix &optional (bad-value nil))
  (let*  ((reg-exp (format nil "[^\\d](\\d{1,3})~a" suffix))
          (result (multiple-value-list
                   (scan-to-strings reg-exp
                                    (format nil " ~a" duration)))))
    (if (not (null (first result)))
        (parse-integer (elt (second result) 0))
        bad-value)))

(defun offset-date-in-past (date duration)
  "Функция парсит указанный в строке временной промежуток и
   вычитает его из date. Примеры промежутка '10s 1m 60h 7D 13M 1Y',
   '10m 6D', '7D 10m 1s'. s - секунды, m - минуты, h - часы, D - дни,
   M - месяцы, Y - года"
  (let ((sec    (parse-duration-value duration "s" 0))
        (minute (parse-duration-value duration "m" 0))
        (hour   (parse-duration-value duration "h" 0))
        (day    (parse-duration-value duration "D" 0))
        (month  (parse-duration-value duration "M" 0))
        (year   (parse-duration-value duration "Y" 0)))
    (timestamp-
     (timestamp-
      (timestamp-
       (timestamp-
        (timestamp-
         (timestamp-
          date sec :sec) minute :minute) hour :hour) day :day) month :month) year :year)))

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

;; Имя файла не соответствует указанному шаблону?
(defcondition file-name-not-match (path pattern)
  (not (file-name-match path pattern)))

;; Условие всегда возвращает истину
(defcondition always-true (path)
  t)

;; Условие всегда возвращает ложь
(defcondition always-false (path)
  nil)

;; Количество времени, прошедшего с момента последней записи в
;; файл, больше указанного лимита?
(defcondition file-age-greater (path limit)
  (let ((file-mod-date
          (universal-to-timestamp (file-write-date path)))
        (limit-date (offset-date-in-past (now) limit)))
    (timestamp< file-mod-date limit-date)))

;; Количество времени, прошедшего с момента последней записи в
;; файл, меньше указанного лимита?
(defcondition file-age-less (path limit)
  (not (age-greater path limit)))
