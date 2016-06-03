(defpackage :rotator
  (:export :main
           :main-loop)
  (:use :common-lisp :cl-fad)
  (:import-from :cl-log
                :log-manager
                :start-messenger
                :text-file-messenger
                :formatted-message
                :log-message)
  (:import-from :rotator.condition
                :file-size-more
                :file-name-match
                :file-size-less)
  (:import-from :rutils
                :with-gensyms)
  (:import-from :rotator.config
                :config-dir-path
                :rules-config-exists?
                :parse
                :rules-config-path))

(in-package :rotator)


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
    ("file-name-match" (file-name-match path limit))
    ("file-size-more"  (file-size-more path limit))
    ("file-size-less"  (file-size-less path limit))))

(defun is-file? (path)
  (if (not (cl-fad:directory-pathname-p path))
      t
      nil))

(defmacro with-item-in-dir (symbol dir-path predicate &body body)
  (rutils:with-gensyms (dir-item)
    `(if (cl-fad:directory-exists-p ,dir-path)
         (dolist (,dir-item (cl-fad:list-directory ,dir-path))
           (if (,predicate ,dir-item)
               (let ((,symbol ,dir-item))
                 ,@body))))))

(defun log-file-path ()
  "Возвращает путь до главного лог-файла"
  (merge-pathnames (config-dir-path) #p"rotator.log"))

(defun init-logger (log-path)
  (setf (log-manager)
        (make-instance 'log-manager :message-class 'formatted-message))
  (start-messenger 'text-file-messenger
                   :filename log-path))

(defun all-conditions-true? (path conditions)
  (not (numberp (position
                 nil
                 (map 'list
                      (lambda (c)
                        (check-condition
                         (gethash :type c)
                         path
                         (gethash :value c)))
                      conditions)))))

(defun main-loop ()
  (let ((directories (parse)))
    (dolist (dir directories)
      (let ((conditions (gethash :conditions dir)))
        (with-item-in-dir file (gethash :path dir) is-file?
          (if (all-conditions-true? file conditions)
              (format t "Rotate ~S~%" (namestring file))))))))

(defun main (argv)
  (declare (ignore argv))
  (cond ((not (rules-config-exists?))
         (format t "Файл ~S не существует!"
                 (namestring (rules-config-path))))
        (t (main-loop))))
