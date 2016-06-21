(defpackage #:rotator.config
  (:use #:cl
        #:cxml
        #:xpath)
  (:import-from :cl-fad
                :file-exists-p
                :directory-exists-p)
  (:import-from :rutils :ensure-keyword)
  (:import-from :cl-log
                :log-message)
  (:import-from :rotator.utils
                :xpath-attr-val)
  (:export :config-dir-path
           :rules-config-path
           :rules-config-exists?
           :parse))

(in-package #:rotator.config)


(defun config-dir-path ()
  "Возвращает путь конфигурационной директории"
  (pathname
   (concatenate 'string
                (directory-namestring (user-homedir-pathname))
                ".rotator/")))

(defun rules-config-path ()
  "Возвращает путь до xml-конфига, где
   указаны параметры ротации"
  (merge-pathnames (config-dir-path) #p"config.xml"))

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

(defun rotator-info (rot-node)
  "Возвращает хэш с информацией о ротаторе, вытащенной
   из xml-конфига"
  (let ((result (make-hash-table)))
    (setf (gethash :params result) (make-hash-table))
    (setf (gethash :id result)
          (xpath:string-value (xpath-attr-val "id" rot-node)))
    (xpath:map-node-set
     (lambda (x) (setf
                  (gethash (ensure-keyword (xpath-attr-val "name" x)) (gethash :params result))
                  (xpath:string-value x)))
     (xpath:evaluate "./param" rot-node))
    result))

(defun condition-info (cond-node)
  "Строит хэш на основании xml узла condition"
  (let ((result (make-hash-table)))
    (setf (gethash :type result) (xpath-attr-val "type" cond-node))
    (setf (gethash :value result) (xpath:string-value cond-node))
    result))

(defun rule-info (rule-node)
  "Возвращает хэш с информацией о правиле ротации. Одна директория
   может содержать несколько правил, внутри которых могут быть
   несколько условий и ротаторов"
  (let ((result (make-hash-table)))
    (setf (gethash :conditions result)
          (xpath:map-node-set->list
           (lambda (x) (condition-info x))
           (xpath:evaluate ".//condition" rule-node)))
    (setf (gethash :rotators result)
          (xpath:map-node-set->list
           (lambda (x) (rotator-info x))
           (xpath:evaluate ".//rotator" rule-node)))
    result))

(defun dir-info (dir-node)
  "Парсит узел-деректорию из xml-конфига. Внутри узла указана
   вся информация для ротации директории."
  (let ((result (make-hash-table)))
    (setf (gethash :path result) (xpath-attr-val "path" dir-node))
    (setf (gethash :rules result)
          (xpath:map-node-set->list
           (lambda (x) (rule-info x))
           (xpath:evaluate ".//rule" dir-node)))
    result))

(defun rotated-directories (document)
  "Возвращает список с информацией о директориях
   указанных в xml-конфиге"
  (xpath:map-node-set->list
   (lambda (node) (dir-info node))
   (xpath:evaluate "//directory" document)))

(defun dir-paths-exists? (root-node)
  "Проверяет на существование все пути к просматриваемым
   директориям в конфиге"
  (every (lambda (x) (not (eql nil x)))
         (xpath:map-node-set->list
          (lambda (dir-node) (directory-exists-p (xpath:string-value dir-node)))
          (xpath:evaluate "//directory/@path" root-node))))

(defun required-attr-exists? (root-node)
  "Проверяет наличие обязательных атрибутов в xml-узлах конфига"
  (and
   (attr-exists? root-node "//directory" "path")
   (attr-exists? root-node "//conditions/condition" "type")
   (attr-exists? root-node "//rotator" "id")))

(defun attr-exists? (root-node selector attr)
  "Проверяет наличие указанного атрибута у выбранных
   по селектору узлов"
  (every (lambda (x) (eql nil x))
         (xpath:map-node-set->list
          (lambda (node) (xpath:node-set-empty-p
                          (xpath:evaluate (concatenate 'string "@" attr) node)))
          (xpath:evaluate selector root-node))))

(defun config-valid (root-node)
  (reduce
   (lambda (acc x)
     (if (eql nil (first x))
         (progn
           (log-message :error (second x))
           nil)
         acc))
   `((,(required-attr-exists? root-node) "Указаны не все обязательные атрибуты!")
     (,(dir-paths-exists? root-node) "Не все указанные пути существуют!"))
   :initial-value t))

(defun parse ()
  "Собственно парсинг xml-конфига. В каждом directory-узле
   содержится ин-ия о методе его ротации. Данная функция возвращает
   список всех указанных в конфиге директорий, со всей ин-ей
   об их ротации"
  (let ((root-node (config-root-element (rules-config-path))))
    (if (and (rules-config-exists?)
             (config-valid root-node))
        (rotated-directories root-node)
        nil)))
