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
	       #:restas-directory-publisher
               #:xfactory
	       #:cl-typesetting
	       #:net-telent-date
	       #:local-time
	       #:clon
	       #:closure-template
	       #:cl-libxml2
	       #:babel
	       #:colorize
	       #:metabang-bind
	       #:iterate
	       #:alexandria
	       #:cl-ppcre)
  :defsystem-depends-on (#:closure-template)
  :components ((:file "pref")
               (:module :src
                        :components ((:file "packages")
                                     (:module "dokuwiki"
                                              :components ((:file "render-html"))
                                              :depends-on ("packages" "wiki" "wiki-parser"))
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
				     (:module "planet"
					      :components ((:file "feed-parser")
							   (:file "spider" :depends-on ("feed-parser"))
							   (:file "planet" :depends-on ("spider")))
					      :depends-on ("packages"))
				     (:module "wiki"
					      :components
					      ((:file "wiki")
					       (:file "storage")
					       (:file "drawer")
					       (:file "routes" :depends-on ("storage" "drawer")))
					      :depends-on ("packages"))
				     (:module "colorize"
					      :components ((:file "defmodule")
							   (:file "storage" :depends-on ("defmodule"))
							   (:file "drawer" :depends-on ("defmodule"))
							   (:file "routes" :depends-on ("storage" "drawer")))
					      :depends-on ("packages"))
				     (:module "forum"
					      :components ((:file "defmodule")
							   (:file "storage" :depends-on ("defmodule"))
							   (:file "forum" :depends-on ("storage")))
					      :depends-on ("packages"))

				     (:module "wiki-parser"
					      :components ((:file "parser")
							   (:file "dokuwiki" :depends-on ("parser")))
					      :depends-on ("packages"))
                                     (:file "storage" :depends-on ("packages"))
                                     (:file "pcl"  :depends-on ("rulisp"))
                                     (:file "jscl"  :depends-on ("rulisp"))
                                     (:file "rulisp" :depends-on ("storage"
								  "dokuwiki"
								  "auth")))
                        :depends-on ("pref"))))
