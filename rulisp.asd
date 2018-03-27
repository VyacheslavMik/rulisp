;;;; rulisp.asd
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem rulisp
  :depends-on (#:restas
	       #:ironclad
	       #:split-sequence
               #:simple-date
	       #:postmodern
               #:zip
	       #:restas-planet
	       #:restas-wiki
               #:restas-colorize
	       #:restas-directory-publisher
	       #:restas-forum
               #:xfactory
	       #:cl-typesetting
               #:wiki-parser)
  :defsystem-depends-on (#:closure-template)
  :components ((:file "pref")
               (:module :src
                        :components ((:file "packages")
                                     (:module "dokuwiki"
                                              :components ((:file "render-html"))
                                              :depends-on ("packages"))
				     (:module "auth"
                                              :components ((:module "templates"
								    :components ((:closure-template "forgot")
										 (:closure-template "login")
										 (:closure-template "register")))
							   (:file "defmodule" :depends-on ("templates"))
							   (:file "storage" :depends-on ("defmodule"))
							   (:file "cookie" :depends-on ("storage"))
							   (:file "sendmail" :depends-on ("defmodule"))
							   (:file "simple-auth" :depends-on ("cookie" "sendmail")))
                                              :depends-on ("packages"))
                                     (:file "storage" :depends-on ("packages"))
                                     (:file "pcl"  :depends-on ("rulisp"))
                                     (:file "jscl"  :depends-on ("rulisp"))
                                     (:file "rulisp" :depends-on ("storage"
								  "dokuwiki"
								  "auth")))
                        :depends-on ("pref"))))

