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
                :rotate
                :info
                :remover)
  (:import-from :rotator.utils :pretty-universal-time)
  (:import-from :rotator.config
                :config-dir-path
                :rules-config-exists?
                :parse
                :rules-config-path))

(in-package :rotator)


(defvar *rotators* (make-hash-table))

(defun create-rotators ()
  "Заполняет глобальный хэш *rotators* всеми доступными
   ротаторами. В качестве ключа выступает идентификатор ротатора"
  (let ((remover-ins (make-instance 'remover))
        (info-ins    (make-instance 'info)))
    (progn
      (setf (gethash (ident info-ins) *rotators*) info-ins)
      (setf (gethash (ident remover-ins) *rotators*) remover-ins))))


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
  (bind-code cond-id nil
    ("file-name-match"     (file-name-match path limit))
    ("file-name-not-match" (file-name-not-match path limit))
    ("file-size-more"      (file-size-more path limit))
    ("always-true"         (always-true path limit))
    ("always-false"        (always-false path limit))
    ("file-age-greater"    (file-age-greater path limit))
    ("file-age-less"       (file-age-less path limit))
    ("file-size-less"      (file-size-less path limit))))

(defun rotate-file (path rotator-id)
  (let ((cur-rotator (gethash rotator-id *rotators*)))
    (if cur-rotator
        (rotate cur-rotator path)
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
  (let ((result t))
    (dolist (c conditions)
      (if (eql nil (check-condition
                    (gethash :type c)
                    path
                    (gethash :value c)))
          (progn
            (setf result nil)
            (return))))
    result))

(defun main-loop ()
  (let ((directories (parse)))
    (dolist (dir directories)
      (let ((conditions (gethash :conditions dir))
            (rotators   (gethash :rotators   dir)))
        (with-item-in-dir file (gethash :path dir) is-file?
          (if (all-conditions-true? file conditions)
              (dolist (r rotators)
                (rotate-file
                 (namestring file)
                 (ensure-keyword (gethash :id r))))))))))

(defun main (argv)
  (declare (ignore argv))
  (cond ((not (rules-config-exists?))
         (format t "Файл ~S не существует!"
                 (namestring (rules-config-path))))
        (t (progn
             (create-rotators)
             (init-logger (log-file-path))
             (main-loop)))))
