(defpackage :rotator.rotator
  (:import-from :cl-log
                :log-message)
  (:import-from :cl-fad
                :file-exists-p)
  (:import-from :rutils
                :sethash)
  (:import-from :local-time
                :format-timestring
                :+iso-8601-format+
                :now)
  (:import-from :cl-rules
                :defaction)
  (:use :common-lisp))

(in-package :rotator.rotator)


(defun unique-file-path (path &optional (index 0))
  "Уникализирует имя файла путем добавления
   к нему суффикса"
  (concatenate 'string
          (directory-namestring path)
          (pathname-name path)
          "."
          (format-timestring nil (now) :format +iso-8601-format+)
          "."
          (write-to-string index)
          "."
          (pathname-type path)))

(defun find-new-file-path (path &optional (attempt 10))
  "Пытается построить такой файловый путь, чтобы файл
   с таким именем не существовал. attempt - количество
   попыток нахождения пути"
  (let ((result nil))
    (dotimes (i attempt)
      (let ((new-path (unique-file-path path i)))
        (if (not (file-exists-p new-path))
            (progn
              (setf result new-path)
              (return)))))
    result))

(defun log-label (ident)
  "Возвращает 'подпись' ротатора для представления в
   лог-файле"
  (format nil "[ROTATOR] (~a)" ident))


(defvar *remove-count* (make-hash-table :test 'equalp)
  "Хранит количество удаленных файлов в каждой директории.")


(defaction remover (path &optional (max-one-shot 100))
  (let ((ident :remover)
        (dir-remove-count (gethash (directory-namestring path) *remove-count*)))
    (when (null dir-remove-count) (setf dir-remove-count 0))
    (when (or (null max-one-shot) (< dir-remove-count max-one-shot))
      (delete-file (pathname path))
      (sethash (directory-namestring path)
               *remove-count*
               (1+ dir-remove-count))
      (log-message :info
                   (format nil "~a Файл ~s успешно удален" (log-label ident) path)))))


(defaction mover (path move-path)
  (let* ((ident :mover)
         (new-path (format nil "~a/~a" move-path (file-namestring path))))
    (handler-case
        (cond
          ((null move-path)
           (log-message :warning
                        (format nil "~a Не указан обязательный параметр path"
                                (log-label ident))))
          ((not (file-exists-p move-path))
           (log-message :warning
                        (format nil "~a Директория ~s не существует"
                                (log-label ident)
                                move-path)))
          (t (progn
               (if (file-exists-p new-path)
                   (setf new-path (find-new-file-path new-path 20)))
               (rename-file path new-path)
               (log-message :info
                            (format nil "~a Файл ~s перемещен в ~s"
                                    (log-label ident)
                                    path
                                    new-path)))))
      (sb-int:simple-file-error (e)
        (log-message :warning
                     (format nil "~a Не удалось переместить ~s в ~s"
                             (log-label ident)
                             path
                             new-path))))))


(defaction info (path)
  (log-message :info
               (format nil "~a Файл ~s был подвергнут ротации" :info path)))
