;;;; packages.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp.colorize)

;;;; load templates

(defparameter *colorize-template-path*
  (merge-pathnames "src/colorize/drawer.tmpl"
                   (asdf:component-pathname (asdf:find-system '#:rulisp))))

(closure-template:compile-template :common-lisp-backend
                                   *colorize-template-path*)

;;;; preferences

(defvar *max-on-page* 10)

(defvar *storage* nil)

(defparameter *colorize-user-function*
  #'(lambda () "anonymous"))

(defun colorize-user ()
  (if *colorize-user-function*
      (funcall *colorize-user-function*)))
