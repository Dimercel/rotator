(defpackage :rotator.condition
  (:export :file-size-more
           :file-size-less
           :file-name-match
           :file-name-not-match
           :file-age-greater
           :file-age-less)
  (:use :common-lisp)
  (:import-from :cl-rules
                :defcond)
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
        ((equal suffix "GB") 1073741824)
        (t 1)))

(defun name-pattern-to-re (expr)
  "В конфиге правил ротации могут быть указаны паттерны
   для имен файлов. Данная функция конвертирует такие
   строки в корректные регулярные выражения"
  (re-begin-and-end-str
   (regex-replace-all "\\*"
                      (regex-replace-all "\\." expr "\\.")
                      ".*")))

(defun parse-duration-value (duration suffix &optional (bad-value nil))
  "Возвращает числовое значение из указанного в строке компонента
   временного промежутка. Поиск компонента происходит по SUFFIX.
   Если промежуток указан не корректно, то вернет BAD-VALUE.
   Например: (parse-duration-value \"10D 30M\" \"M\") => 30
             (parse-duration-value \"20D\" \"M\") => nil"
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
  (reduce (lambda (acc x)
            (timestamp- acc (first x) (second x)))
           (map 'list
                (lambda (x)
                  (list (parse-duration-value duration (first x) 0) (second x)))
                '(("s" :sec)
                  ("m" :minute)
                  ("h" :hour)
                  ("D" :day)
                  ("M" :month)
                  ("Y" :year)))
          :initial-value date))


;;; Определение условий ротации


(defun file-size-more (path limit)
  (let ((limit-value (size-from-text limit)))
    (if (and
         limit-value
         (scan "\\d+(KB|MB|GB|B)?" limit))
        (> (file-size (pathname path)) limit-value)
        nil)))

;; Файл больше указанного размера?
(defcond file-size-more (path limit)
  (file-size-more path limit))

(defun file-size-less (path limit)
  (not (file-size-more path limit)))

;; Файл меньше указанного размера?
(defcond file-size-less (path limit)
  (file-size-less path limit))

(defun file-name-match (path pattern)
  (if (scan (name-pattern-to-re pattern) (file-namestring path))
      t
      nil))

;; Имя файла соответствует указанному шаблону?
(defcond file-name-match (path pattern)
  (file-name-match path pattern))

(defun file-name-not-match (path pattern)
  (not (file-name-match path pattern)))

;; Имя файла не соответствует указанному шаблону?
(defcond file-name-not-match (path pattern)
  (file-name-not-match path pattern))

(defun file-age-greater (path limit)
  (let ((file-mod-date
          (universal-to-timestamp (file-write-date path)))
        (limit-date (offset-date-in-past (now) limit)))
    (timestamp< file-mod-date limit-date)))

;; Количество времени, прошедшего с момента последней записи в
;; файл, больше указанного лимита?
(defcond file-age-greater (path limit)
  (file-age-greater path limit))

(defun file-age-less (path limit)
  (not (file-age-greater path limit)))

;; Количество времени, прошедшего с момента последней записи в
;; файл, меньше указанного лимита?
(defcond file-age-less (path limit)
  (file-age-less path limit))

;; Условие всегда возвращает истину
(defcond always-true (path)
  (or path t))

;; Условие всегда возвращает ложь
(defcondition always-false (path)
  (and path nil))
