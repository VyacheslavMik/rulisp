;;;; defmodule.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp.forum)

(defparameter *storage* nil)

(defparameter *site-name* nil)

(defparameter *rss-item-count* 20)

(defparameter *max-topic-on-page* 10)

(defparameter *max-reply-on-page* 50)

(defparameter *user-name-function* nil)

(defparameter *rulisp-pathname* (asdf:component-pathname (asdf:find-system '#:rulisp)))

(restas:mount-module resources (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* (merge-pathnames "resources/" *rulisp-pathname*))
  (restas.directory-publisher:*autoindex* nil))

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/forum/forum.tmpl"
                                                    *rulisp-pathname*))
