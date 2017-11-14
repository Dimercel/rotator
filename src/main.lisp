(defpackage :rotator
  (:export :main)
  (:use :common-lisp
        :cl-fad
        :cl-log
        :rotator.rotator
        :rotator.condition)
  (:import-from :rutils
                :with-gensyms
                :ensure-keyword)
  (:import-from :cl-rules
                :defparam
                :setparam
                :eval-rule)
  (:import-from :alexandria
                :hash-table-keys)
  (:import-from :rotator.utils :pretty-universal-time)
  (:import-from :rotator.config
                :config-dir-path
                :load-rules
                :rules-config-path
                :rules-config-exists-p
                :watch-directories))

(in-package :rotator)


(defparam watch-path "")

;; Настройка параметров логгирования
(defun log-file-path ()
  "Возвращает путь до главного лог-файла"
  (merge-pathnames (config-dir-path) #p"rotator.log"))


(defclass rotator-base-message (cl-log:formatted-message)
  ())

(defmethod format-message ((self rotator-base-message))
  (format nil "~a ~a ~?~&"
          (pretty-universal-time
           (cl-log:timestamp-universal-time
            (cl-log:message-timestamp self)))
          (cl-log:message-category self)
          (cl-log:message-description self)
          (cl-log:message-arguments self)))

(defun init-logger (log-path)
  "Инициализирует логгер со специфичным форматом сообщений"
  (setf (cl-log:log-manager)
        (make-instance 'cl-log:log-manager :message-class 'rotator-base-message))
  (cl-log:start-messenger 'cl-log:text-file-messenger
                          :filename log-path))


(defun is-file-p (path)
  (if (not (cl-fad:directory-pathname-p path))
      t
      nil))

(defmacro with-item-in-dir (symbol dir-path predicate &body body)
  "Проходит все содержимое указанной директории DIR-PATH и подставляет
   в SYMBOL элемент, если выполняется предикат PREDICATE"
  (with-gensyms (dir-item)
    `(if (cl-fad:directory-exists-p ,dir-path)
         (dolist (,dir-item (cl-fad:list-directory ,dir-path))
           (when (,predicate ,dir-item)
               (let ((,symbol ,dir-item))
                 ,@body))))))


(defun main-loop ()
  "Обходит все отслеживаемые директории и
   выполняет в них ротацию"
  (let ((watch-dirs (watch-directories)))
    (dolist (dir-path (hash-table-keys watch-dirs))
      (with-item-in-dir file dir-path is-file-p
        (setparam 'watch-path (namestring file))
        (eval-rule (gethash dir-path watch-dirs))))))

(defun main (argv)
  (declare (ignore argv))
  (cond ((not (rules-config-exists-p))
         (format t "Файл ~S не существует!"
                 (namestring (rules-config-path))))
        (t (progn
             (init-logger (log-file-path))
             (load-rules)
             (main-loop)))))
