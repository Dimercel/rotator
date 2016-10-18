(defpackage :rotator
  (:export :main)
  (:use :common-lisp :cl-fad :cl-log)
  (:import-from :rutils
                :with-gensyms
                :ensure-keyword)
  (:import-from :rotator.condition
                :file-size-more
                :file-size-less
                :file-name-match
                :file-name-not-match
                :file-age-greater
                :file-age-less
                :always-true
                :always-false)
  (:import-from :rotator.rotator
                :ident
                :params
                :import-raw-params
                :rotate
                :info
                :mover
                :remover)
  (:import-from :rotator.utils :pretty-universal-time)
  (:import-from :rotator.config
                :config-dir-path
                :config-root-element
                :rules-config-exists?
                :rules-config-path
                :directories
                :rules
                :directory-path
                :conditions
                :condition-type
                :condition-value
                :rotators
                :rotator-id
                :rotator-params
                :rules-config-path))

(in-package :rotator)


(defvar *rotators* (make-hash-table))

(defun create-rotators ()
  "Заполняет глобальный хэш *rotators* всеми доступными
   ротаторами. В качестве ключа выступает идентификатор ротатора"
  (let ((remover-ins (make-instance 'remover))
        (info-ins    (make-instance 'info))
        (mover-ins   (make-instance 'mover)))
    (progn
      (setf (gethash (ident info-ins) *rotators*) info-ins)
      (setf (gethash (ident remover-ins) *rotators*) remover-ins)
      (setf (gethash (ident mover-ins) *rotators*) mover-ins))))

;; Нижеследующий макрос связывает значение с каким-либо кодом.
;; Такая необходимость нужна когда поток выполнения зависит
;; от внешних данных. Например мы можем связать имя расчета
;; указанного в конфиге с кодом его вычисления
(defmacro bind-code (value bad-value &body forms)
  `(cond
       ,@(loop for f in forms collect
               `((equal ,value ,(first f)) ,(second f)))
       (t ,bad-value)))

(defun check-condition (cond-id path limit)
  "Собственно проверка на выполнение условия."
  (bind-code cond-id nil
    ("file-name-match"     (file-name-match path limit))
    ("file-name-not-match" (file-name-not-match path limit))
    ("file-size-more"      (file-size-more path limit))
    ("always-true"         (always-true path limit))
    ("always-false"        (always-false path limit))
    ("file-age-greater"    (file-age-greater path limit))
    ("file-age-less"       (file-age-less path limit))
    ("file-size-less"      (file-size-less path limit))))

(defun rotate-file (path rotator-id &optional (parameters nil))
  "Осуществляет ротацию файла указанным ротатором."
  (let ((cur-rotator (gethash rotator-id *rotators*)))
    (if cur-rotator
        (progn
          (import-raw-params cur-rotator parameters)
          (rotate cur-rotator path))
        (log-message
         :warning
         (format nil
                 "Ротатор с идентификатором ~S не существует!"
                 rotator-id)))))

(defun is-file? (path)
  (if (not (cl-fad:directory-pathname-p path))
      t
      nil))

(defmacro with-item-in-dir (symbol dir-path predicate &body body)
  (with-gensyms (dir-item)
    `(if (cl-fad:directory-exists-p ,dir-path)
         (dolist (,dir-item (cl-fad:list-directory ,dir-path))
           (if (,predicate ,dir-item)
               (let ((,symbol ,dir-item))
                 ,@body))))))

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
  (setf (cl-log:log-manager)
        (make-instance 'cl-log:log-manager :message-class 'rotator-base-message))
  (cl-log:start-messenger 'cl-log:text-file-messenger
                   :filename log-path))

(defun all-conditions-true? (path conditions)
  (every (lambda (x)
           (check-condition (condition-type x)
                            path
                            (condition-value x)))
         conditions))

(defun main-loop ()
  (let ((root (config-root-element (rules-config-path))))
    (dolist (dir (directories root))
      (dolist (rule (rules dir))
        (with-item-in-dir file (directory-path dir) is-file?
          (when (all-conditions-true? file (conditions rule))
            (dolist (r (rotators rule))
              (rotate-file
               (namestring file)
               (ensure-keyword (rotator-id r))
               (rotator-params r)))))))))

(defun main (argv)
  (declare (ignore argv))
  (cond ((not (rules-config-exists?))
         (format t "Файл ~S не существует!"
                 (namestring (rules-config-path))))
        (t (progn
             (create-rotators)
             (init-logger (log-file-path))
             (main-loop)))))
