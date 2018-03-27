;;;; planet.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:rulisp.planet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; compile view templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (closure-template:compile-template :common-lisp-backend
                                     (merge-pathnames "src/planet/planet.tmpl"
                                                      (asdf:component-pathname (asdf:find-system '#:rulisp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *name* "PLANET")

(defvar *feeds* nil)

(defvar *spider* nil)

(defvar *suggest-mail* nil)

(defvar *schedule* '(:hour *))

(defvar *template* 'rulisp.planet.view:feed-html)

(defvar *cache-dir* nil)

(defmethod restas:initialize-module-instance ((module (eql #.*package*)) context)
  (restas:with-context context
    (when *feeds*
      (restas:context-add-variable context
                                   '*spider*
                                   (make-instance 'spider
                                                  :feeds *feeds*
                                                  :schedule *schedule*
                                                  :cache-dir (if *cache-dir*
                                                                 (ensure-directories-exist (merge-pathnames "spider/"
                                                                                                            *cache-dir*))))))))

(defmethod restas:finalize-module-instance ((module (eql #.*package*)) context)
  (let ((spider (restas:context-symbol-value context '*spider*)))
    (when spider
      (spider-stop-scheduler spider))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *resource-dir*
  (merge-pathnames "resources/planet/"
                   (asdf:component-pathname (asdf:find-system '#:rulisp))))

(defun planet-path (path)
  (merge-pathnames path *resource-dir*))

(restas:define-route planet-resources (":(file)")
  (planet-path file))

(defun prepare-planet-data ()
  (list :entry-list (spider-syndicate-feed *spider*)
        :authors (spider-feeds-authors *spider*)
        :css (list (restas:genurl 'planet-resources :file "planet.css"))
        :href-atom (restas:genurl* 'planet-atom)
        :href-html (restas:genurl* 'planet-main)
        :name *name*
        :suggest-mail *suggest-mail*))

(restas:define-route planet-atom ("atom.xml")
  (:content-type "application/atom+xml")
  (:render-method 'rulisp.planet.view:atom-feed)
  (prepare-planet-data))

(restas:define-route planet-main ("")
  (:render-method #'(lambda (data) (funcall *template* data)))
  (prepare-planet-data))

