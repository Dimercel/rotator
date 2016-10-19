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
  (:import-from :xpath
                :all-nodes
                :evaluate
                :map-node-set->list
                :string-value)
  (:export :config-dir-path
           :rules-config-path
           :rules-config-exists?
           :config-root-element
           :directories
           :directory-path
           :rules
           :conditions
           :condition-value
           :condition-type
           :rotators
           :rotator-id
           :rotator-params
           :param-name
           :param-value
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

(defun node-value (node)
  "Возвращает текст xml-узла"
  (string-value node))

(defun directories (root-node)
  "Возвращает список со всеми отслеживаемыми
   директориями."
  (all-nodes
   (evaluate "./directory" root-node)))

(defun directory-path (dir-node)
  "Возвращает значение атрибута path у директории"
  (xpath-attr-val "path" dir-node))

(defun rules (dir-node)
  "Возвращает список правил, активных для директории. Каждое
   правило может содержать набор условий и ротаторов"
  (all-nodes
   (evaluate "./rule" dir-node)))

(defun conditions (rule-node &optional (type nil))
  "Извлекает все условия из узла-правила"
  (let ((all-conditions (all-nodes
                         (evaluate "./conditions/condition" rule-node))))
    (if (null type)
        all-conditions
        (remove-if-not (lambda (x) (equal (xpath-attr-val "type" x)
                                          type))
                       all-conditions))))

(defun condition-type (cond-node)
  "Возвращает тип-идентификатор условия"
  (xpath-attr-val "type" cond-node))

(defun condition-value (cond-node)
  "Значение лимита в условии"
  (node-value cond-node))

(defun rotators (rule-node &optional (id nil))
  "Извлекает все ротаторы из узла-правила"
  (let ((all-rotators (all-nodes
                       (evaluate "./rotator" rule-node))))
    (if (null id)
        all-rotators
        (remove-if-not (lambda (x) (equal (xpath-attr-val "id" x)
                                          id))
                       all-rotators))))

(defun rotator-id (rotator-node)
  "Собственно идентификтор ротатора"
  (xpath-attr-val"id" rotator-node))

(defun rotator-params (rotator-node)
  "Возвращает все параметры, указанные
   в ротаторе"
  (all-nodes
   (evaluate "./param" rotator-node)))

(defun param-name (param-node)
  ""
  (xpath-attr-val "name" param-node))

(defun param-value (param-node)
  ""
  (node-value param-node))


(defun dir-paths-exists? (root-node)
  "Проверяет на существование все пути к просматриваемым
   директориям в конфиге"
  (every (lambda (x) (not (eql nil x)))
         (map-node-set->list
          (lambda (dir-node) (directory-exists-p (string-value dir-node)))
          (evaluate "//directory/@path" root-node))))

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
         (map-node-set->list
          (lambda (node) (node-set-empty-p
                          (evaluate (concatenate 'string "@" attr) node)))
          (evaluate selector root-node))))

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
   содержится ин-ия о методе его ротации."
  (let ((root-node (config-root-element (rules-config-path))))
    (if (and (rules-config-exists?)
             (config-valid root-node))
        root-node
        nil)))
