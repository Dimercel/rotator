(defpackage :rotator
  (:export :main
           :config-dir
           :rules-config-exists?
           :config-root-element
           :monitored-directories
           :rotator-info
           :log-file-path
           :init-logger
           :rules-config-path)
  (:use :common-lisp
        :cxml
        :xpath
        :rotator.condition)
  (:import-from :cl-fad :file-exists-p)
  (:import-from :cl-log
                :log-manager
                :start-messenger
                :text-file-messenger
                :formatted-message
                :log-message)
  (:import-from :cl-ppcre :scan))

(in-package :rotator)


(defun xpath-attr-val (attr-name node)
  "Возвращает значение атрибута указанного xml узла"
  (xpath:string-value
   (xpath:evaluate
    (concatenate 'string "@" attr-name)
    node)))

(defun config-dir ()
  "Возвращает путь конфигурационной директории"
  (pathname
   (concatenate 'string
                (directory-namestring (user-homedir-pathname))
                ".rotator/")))

(defun log-file-path ()
  "Возвращает путь до главного лог-файла"
  (merge-pathnames (config-dir) #p"rotator.log"))

(defun init-logger (log-path)
  (setf (log-manager)
        (make-instance 'log-manager :message-class 'formatted-message))
  (start-messenger 'text-file-messenger
                   :filename log-path))

(defun rules-config-path ()
  "Возвращает путь до xml-конфига, где
   указаны параметры ротации"
  (merge-pathnames (config-dir) #p"config.xml"))

(defun rules-config-exists? ()
  "Проверка на существование файла-конфига"
  (if (file-exists-p (rules-config-path))
      t
      nil))

(defun config-root-element (path)
  "Возвращает корневой узел xml-конфига"
  (dom:document-element 
   (cxml:parse-file path
                    (cxml-dom:make-dom-builder))))

(defun monitored-directories (document)
  "Получить ноды всех отслеживаемых директорий,
   указанных в xml-конфиге"
  (xpath:evaluate "//directory" document))

(defun rotator-info (dir-elem)
  "Возвращает хэш с информацией о ротаторе, вытащенной
   из xml-конфига"
  (let ((result (make-hash-table)))
    (setf (gethash "name" result)
          (xpath:string-value (xpath:evaluate "//rotator/@name" dir-elem)))
    (xpath:map-node-set
     (lambda (x) (setf
                  (gethash (xpath-attr-val "name" x) result)
                  (xpath:string-value x)))
     (xpath:evaluate "//rotator/param" dir-elem))
    result))

(defun main (argv)
  (declare (ignore argv))
  (log-message :info "xolcman")
  (print "It's work"))
