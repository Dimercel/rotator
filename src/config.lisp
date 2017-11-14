(defpackage :rotator.config
  (:export :config-valid-p
           :load-rules
           :rules-config-path
           :rules-config-exists-p
           :watch-directories)
  (:use :cl :cl-yaml)
  (:import-from :cl-fad
                :directory-exists-p
                :file-exists-p)
  (:import-from :alexandria
                :define-constant
                :hash-table-keys)
  (:import-from :cl-rules
                :loads))

(in-package #:rotator.config)


(define-constant +watch-dir-key+ "directories" :test #'equalp
  :documentation
  "Имя ключа в конфиге, который содержит информацию о директориях
   подверженных ротации")


(defun config-dir-path ()
  "Возвращает путь конфигурационной директории"
  (pathname
   (concatenate 'string
                (directory-namestring (user-homedir-pathname))
                ".rotator/")))

(defun rules-config-path ()
  "Возвращает путь до yml-конфига, где
   указаны параметры ротации"
  (merge-pathnames (config-dir-path) #p"config.yml"))

(defun rules-config-exists-p ()
  "Проверка на существование файла-конфига"
  (if (file-exists-p (rules-config-path))
      t
      nil))

(defun watch-directories ()
  "Вернет хеш, ключами которого будут являться пути
   директорий, подверженных ротации. Значения этого
   хеша содержат список имен правил для ротации"
  (gethash +watch-dir-key+ (parse (rules-config-path))))


(defun load-rules ()
  "Используя cl-rules подгружаем все правила
   указанные в yml-конфиге"
  (loads (rules-config-path)))

(defun contain-watch-dirs-p (data)
  "В конфиге присутствует раздел с описанием
   директорий для ротации?"
  (if (gethash +watch-dir-key+ data)
      t
      nil))

(defun all-watch-dirs-exists-p (data)
  (every #'directory-exists-p
         (hash-table-keys (gethash +watch-dir-key+ data))))

(defun config-valid-p (data)
  "Проверяет yml-конфиг на валидность. В DATA
   должна быть передана структура, которую
   возвращает yaml-парсер"
  (every (lambda (x) (equalp t x))
         (list
          (contain-watch-dirs-p data)
          (all-watch-dirs-exists-p data))))
