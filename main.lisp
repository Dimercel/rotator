(defpackage :rotator
  (:export :test
           :config-dir
           :rules-config-exist?
           :rules-config-path)
  (:use :common-lisp :cxml))

(in-package :rotator)

(defun test ()
  "test")

(defun config-dir ()
  (pathname
   (concatenate 'string
                (directory-namestring (user-homedir-pathname))
                ".rotator/")))

(defun rules-config-path ()
  (merge-pathnames (config-dir) #p"config.xml"))

(defun rules-config-exist? ()
  (let ((exist (probe-file (rules-config-path))))
    (if exist
        T
        nil)))
