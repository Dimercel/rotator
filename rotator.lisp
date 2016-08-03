(defpackage :rotator.rotator
  (:export :remover
           :mover
           :ident
           :info
           :params
           :import-raw-params
           :rotate)
  (:import-from :cl-log
                :log-message)
  (:import-from :cl-fad
                :file-exists-p)
  (:import-from :rutils
                :print-hash-table
                :sethash)
  (:use :common-lisp))

(in-package :rotator.rotator)


(defun unique-file-path (path &optional (index 0))
  "Уникализирует имя файла путем добавления
   к нему суффикса"
  (concatenate 'string
          (directory-namestring path)
          (pathname-name path)
          "."
          (write-to-string index)
          "."
          (pathname-type path)))

(defun find-new-file-path (path &optional (attempt 10))
  (let ((result nil))
    (dotimes (i attempt)
      (let ((new-path (unique-file-path path i)))
        (if (not (file-exists-p new-path))
            (progn
              (setf result new-path)
              (return)))))
    result))

(defclass rotator ()
  ((ident
    :reader ident)
   (params
    :initarg :params
    :accessor params
    :initform nil)))

(defgeneric rotate (rotator path)
  (:documentation "Здесь происходит ротация файла,
   указанного в path"))


(defgeneric import-raw-params (rotator raw-params)
  (:documentation "Импортирует значения параметров ротатора
   из обычного хеша"))

(defun log-label (rotator)
  "Возвращает 'подпись' ротатора для представления в
   лог-файле"
  (format nil "[ROTATOR] (~a)" (ident rotator) ))


(defclass remover (rotator)
  ((remove-count
    :reader remove-count
    :initform (make-hash-table :test 'equal))))

(defmethod initialize-instance :after ((self remover) &key)
  (setf (slot-value self 'ident) :remover)
  (setf (slot-value self 'params) (make-hash-table))
  (sethash :max-one-shot
           (slot-value self 'params) 100))


(defmethod import-raw-params ((self remover) raw-params)
  (setf (params self) (make-hash-table))
  (when (gethash :max-one-shot raw-params)
    (sethash :max-one-shot
             (params self)
             (parse-integer (gethash :max-one-shot raw-params) :junk-allowed t))))

(defmethod rotate ((self remover) path)
  (let ((max-one-shot (gethash :max-one-shot (params self)))
        (dir-remove-count
          (gethash (directory-namestring path) (remove-count self))))
    (when (null dir-remove-count) (setf dir-remove-count 0))
    (when (or (null max-one-shot) (< dir-remove-count max-one-shot))
      (delete-file (pathname path))
      (sethash (directory-namestring path)
               (remove-count self)
               (1+ dir-remove-count))
      (log-message :info
                   (format nil "~a Файл ~s успешно удален" (log-label self) path)))))


(defclass mover (rotator)
  ())

(defmethod initialize-instance :after ((self mover) &key)
  (setf (slot-value self 'ident) :mover)
  (setf (slot-value self 'params) (make-hash-table)))

(defmethod import-raw-params ((self mover) raw-params)
  nil)

(defmethod rotate ((self mover) path)
  (let* ((move-path (gethash :path (params self)))
         (new-path (format nil "~a/~a" move-path (file-namestring path))))
    (handler-case
        (cond
          ((null move-path)
           (log-message :warning
                        (format nil "~a Не указан обязательный параметр path"
                                (log-label self))))
          ((not (file-exists-p move-path))
           (log-message :warning
                        (format nil "~a Директория ~s не существует"
                                (log-label self)
                                move-path)))
          (t (progn
               (if (file-exists-p new-path)
                   (setf new-path (find-new-file-path new-path 20)))
               (rename-file path new-path)
               (log-message :info
                            (format nil "~a Файл ~s перемещен в ~s"
                                    (log-label self)
                                    path
                                    new-path)))))
      (sb-int:simple-file-error (e)
        (log-message :warning
                     (format nil "~a Не удалось переместить ~s в ~s"
                             (log-label self)
                             path
                             new-path))))))


(defclass info (rotator)
  ())

(defmethod initialize-instance :after ((self info) &key)
  (setf (slot-value self 'ident) :info))

(defmethod import-raw-params ((self info) raw-params)
  nil)

(defmethod rotate ((self info) path)
    (log-message :info
                 (format nil "~a Файл ~s был подвергнут ротации" (log-label self) path)))
