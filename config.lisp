(defpackage #:rotator.config
  (:use #:cl
        #:cxml
        #:xpath)
  (:import-from :cl-fad :file-exists-p)
  (:import-from :rotator.utils
                :xpath-attr-val)
  (:export :config-dir))

(in-package #:rotator.config)


(defun config-dir ()
  "Возвращает путь конфигурационной директории"
  (pathname
   (concatenate 'string
                (directory-namestring (user-homedir-pathname))
                ".rotator/")))

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

(defun rotator-info (rot-node)
  "Возвращает хэш с информацией о ротаторе, вытащенной
   из xml-конфига"
  (let ((result (make-hash-table)))
    (setf (gethash "id" result)
          (xpath:string-value (xpath-attr-val "id" rot-node)))
    (xpath:map-node-set
     (lambda (x) (setf
                  (gethash (xpath-attr-val "name" x) result)
                  (xpath:string-value x)))
     (xpath:evaluate "//param" rot-node))
    result))

(defun condition-info (cond-node)
  "Строит хэш на основании xml узла condition"
  (let ((result (make-hash-table)))
    (setf (gethash "type" result) (xpath-attr-val "type" cond-node))
    (setf (gethash "value" result) (xpath:string-value cond-node))
    result))
