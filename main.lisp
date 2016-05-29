(defpackage :rotator
  (:export :main
           :test-bind
           :log-file-path
           :init-logger)
  (:use :common-lisp)
  (:import-from :cl-log
                :log-manager
                :start-messenger
                :text-file-messenger
                :formatted-message
                :log-message)
  (:import-from :rotator.config
                :config-dir-path))

(in-package :rotator)


;; Нижеследующий макрос связывает значение с каким-либо кодом.
;; Такая необходимость нужна когда поток выполнения зависит
;; от внешних данных. Например мы можем связать имя расчета
;; указанного в конфиге с кодом его вычисления
(defmacro bind-code (name &body forms)
  `(defun ,name (value def-value)
     (cond
       ,@(loop for f in forms collect
               `((= value ,(first f)) ,(second f)))
       (t def-value))))

(defun log-file-path ()
  "Возвращает путь до главного лог-файла"
  (merge-pathnames (config-dir-path) #p"rotator.log"))

(defun init-logger (log-path)
  (setf (log-manager)
        (make-instance 'log-manager :message-class 'formatted-message))
  (start-messenger 'text-file-messenger
                   :filename log-path))

(defun main (argv)
  (declare (ignore argv))
  (print "It's work"))
