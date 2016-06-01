(defpackage :rotator
  (:export :main
           :log-file-path
           :bind-code
           :check-condition
           :loop
           :init-logger)
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

(defun loop ()
  (let ((directories (parse)))
    (dolist (dir directories)
      (dolist (file (cl-fad:list-directory (gethash :path dir)))
        (let ((conditions (gethash :conditions dir)))
          (if (not (cl-fad:directory-pathname-p file))
              (if (all-conditions-true? file conditions)
                  (format t "Rotate ~S~%" (namestring file)))))))))

(defun main (argv)
  (declare (ignore argv))
  (cond ((not (rules-config-exists?))
         (format t "Файл ~S не существует!"
                 (namestring (rules-config-path))))
        (t (print "It's work!"))))
