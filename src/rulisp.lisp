;;;; rulisp.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compute login
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-user-login-name ()
  (labels ((find-upper-module (module)
             (let ((parent (restas::module-parent module)))
               (if parent
                   (find-upper-module parent)
                   module))))
    (let ((rulisp-module (find-upper-module restas:*module*)))
      (restas::with-module (gethash '-auth- (slot-value rulisp-module 'restas::children))
        (rulisp.auth::compute-user-login-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rulisp templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *resources-dir*
    (merge-pathnames "resources/"
                     (asdf:component-pathname (asdf:find-system '#:rulisp))))

  (closure-template:compile-template :common-lisp-backend
                                     (merge-pathnames "rulisp.tmpl"
                                                      *resources-dir*)))

(defparameter *mainmenu* `(("Главная" main)
                           ("Статьи" -articles-.main-wiki-page)
                           ("Планета" -planet-.planet-main)
                           ("Форум" -forum-.list-forums)
                           ("Сервисы" tools-list)
                           ("Practical Common Lisp" -pcl-.pcl-main)
                           ("Wiki" -wiki-.main-wiki-page)
                           ("Поиск" google-search)))

(defun rulisp-finalize-page (&key title css js content)
  (rulisp.view:main-frame 
   (list :title title
         :css (iter (for item in css)
                    (collect (format nil "/css/~A" item)))
         :js js
         :gecko-png  "/image/gecko.png"
         :user (compute-user-login-name)
         :main-menu (iter (for item in *mainmenu*)
                          (collect (list :href (apply #'restas:genurl (second item) (cddr item))
                                         :name (first item))))
         :content content
         :callback (hunchentoot:request-uri*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route main ("")
  (rulisp-finalize-page :title "Русскоязычное сообщество Common Lisp разработчиков"
                        :css '("style.css")
                        :content (alexandria:read-file-into-string (merge-pathnames "index.html"
                                                                                    *resources-dir*))))

(restas:define-route tools-list ("apps/")
  (rulisp-finalize-page :title "Инструменты"
                        :css '("style.css")
                        :content (rulisp.view:tools)))

(restas:define-route google-search ("search")
  (rulisp-finalize-page :title "Поиск по сайту Lisper.ru"
                        :css '("style.css")
                        :content (rulisp.view:google-search)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rulisp-drawer () ())

(defmethod restas:render-object ((designer rulisp-drawer) (code (eql hunchentoot:+http-not-found+)))
  (rulisp-finalize-page :title "Not Found"
                        :css '("style.css")
                        :content (rulisp.view:not-found-content (list :href (hunchentoot:request-uri*)))))
;;;; pcl

(restas:mount-module -pcl- (#:rulisp.pcl)
  (:url "pcl"))

;; ;;;; auth

(restas:mount-module -auth- (#:rulisp.auth)
  (rulisp.auth:*datastore* *rulisp-db-storage*)
  (rulisp.auth:*noreply-email* *noreply-mail-account*)
  (rulisp.auth:*cookie-cipher-key* *cookie-cipher-key*)
  (rulisp.auth:*finalize-page* (lambda (content)
				 (rulisp-finalize-page :title (getf content :title)
						       :css '("style.css")
						       :content (getf content :body)))))

;;;; forum

(restas:mount-module -forum- (#:rulisp.forum)
  (:url "forum")
  (:render-method
   (lambda (obj)
     (rulisp-finalize-page :title (getf obj :title)
                           :content (restas:render-object (find-package '#:rulisp.forum.view)
                                                          obj)
                           :css '("style.css" "jquery.wysiwyg.css" "forum.css" "colorize.css" )
                           :js (getf obj :js))))
  (rulisp.forum:*site-name* "Lisper.ru")
  (rulisp.forum:*storage* *rulisp-db-storage*)
  (rulisp.forum:*user-name-function* #'compute-user-login-name))

;;;; format

(defclass pastebin-drawer (rulisp.colorize::drawer) ())

(defmethod rulisp.colorize::finalize-page ((drawer pastebin-drawer) data)
  (rulisp-finalize-page  :title (getf data :title)
                         :css '("style.css" "colorize.css")
                         :content (concatenate 'string
                                               (getf data :menu)
                                               (getf data :content))))


(restas:mount-module -format- (#:rulisp.colorize)
  (:url "apps/format/")
  (:render-method (make-instance 'pastebin-drawer))
  (rulisp.colorize:*max-on-page* 15)
  (rulisp.colorize:*storage* *rulisp-db-storage*)
  (rulisp.colorize:*colorize-user-function* #'compute-user-login-name))

;;;; jscl

(restas:mount-module -jscl- (#:rulisp.jscl)
  (:url "apps/jscl/"))

;;;; wiki

(defclass drawer (dokuwiki-drawer) ())

(defmethod rulisp.wiki:finalize-page ((drawer drawer) content)
  (rulisp-finalize-page :title (getf content :title)
                        :css '("style.css" "wiki.css" "colorize.css")
                        :content (concatenate 'string
                                              (rulisp.wiki.view:show-page-menu (getf content :menu-links))
                                              (getf content :content))))


(restas:mount-module -wiki- (#:rulisp.wiki)
  (:url "wiki")
  (:render-method (make-instance 'drawer))
  (rulisp.wiki:*storage* (make-instance 'rulisp.wiki:file-storage :dir *wiki-dir*))
  (rulisp.wiki:*wiki-user-function* #'compute-user-login-name))

;;;; articles

(restas:mount-module -articles- (#:rulisp.wiki)
  (:url "articles")
  (:render-method (make-instance 'drawer))
  (rulisp.wiki:*index-page-title* "Статьи")
  (rulisp.wiki:*storage* (make-instance 'rulisp.wiki:file-storage
                                        :dir #P"/var/rulisp/articles/"))
  (rulisp.wiki:*wiki-user-function* #'(lambda ()
                                        (find (compute-user-login-name)
                                              '("archimag" "dmitry_vk")
                                              :test #'string=))))

;;;; Russian Lisp Planet

(restas:mount-module -planet- (#:rulisp.planet)
  (:url "planet")
  (rulisp.planet:*suggest-mail* "archimag@lisper.ru")
  (rulisp.planet:*feeds* (merge-pathnames "planet-feeds.lisp" *rulisp-path*))
  (rulisp.planet:*name* "Russian Lisp Planet")  
  (rulisp.planet:*cache-dir* (merge-pathnames "planet/" *cachedir*))
  (rulisp.planet:*template* (lambda (data)
                              (rulisp-finalize-page :title "Russian Lisp Planet"
                                                    :css '("style.css" "planet.css" "colorize.css")
                                                    :content (rulisp.planet.view:feed-html-body data)))))

;;;; static files

(eval-when (:compile-toplevel :load-toplevel :execute)
  (closure-template:ensure-ttable-package
   '#:rulisp.directory-publisher.view
   :prototype (closure-template:package-ttable '#:restas.directory-publisher.view))

  (let ((ttable (closure-template:package-ttable '#:rulisp.directory-publisher.view)))
    (flet ((rulisp-autoindex (data out)
             (write-string (rulisp-finalize-page :title (getf data :title)
                                                 :css '("style.css" "autoindex.css")
                                                 :content (restas.directory-publisher.view:autoindex-content data))
                           out)))
      (closure-template:ttable-register-template ttable "AUTOINDEX" #'rulisp-autoindex :supersede t)
      ;;(closure-template:ttable-sync-package ttable '#:rulisp.directory-publisher.view)
      )))

(defmacro defstatic (resource)
  (let* ((module-name (make-symbol (format nil "-STATIC-~a" resource)))
	(resource-string (string-downcase (symbol-name resource)))
	(path (format nil "static/~(~s~)/" resource)))
    `(restas:mount-module ,module-name (#:restas.directory-publisher)
       (:url ,resource-string)
       (restas.directory-publisher:*directory* (merge-pathnames ,path *resources-dir*)))))

(defstatic css)
(defstatic image)
(defstatic js)
(defstatic fonts)

;;;; not found page
(restas:define-route not-found ("*any")
  (:render-method (make-instance 'rulisp-drawer))
  (declare (ignore any))
  hunchentoot:+http-not-found+)
