(defpackage :rotator
  (:export :main
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
