;;;; packages.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp.wiki)

(defvar *storage*)

(defparameter *index-page-title* "index")

(defparameter *wiki-dir*
  (asdf:component-pathname (asdf:find-system '#:rulisp)))

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/wiki/drawer.tmpl"
                                                    *wiki-dir*))

(defvar *wiki-user-function* nil)

(defun wiki-user ()
  (if *wiki-user-function*
      (funcall *wiki-user-function*)))
