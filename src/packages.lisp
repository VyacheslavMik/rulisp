;;;; packages.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(restas:define-module #:rulisp
  (:use #:cl #:iter #:rulisp.preferences)
  (:export #:substring
           #:username

           #:*re-email-check*

           #:send-mail
           #:send-noreply-mail
           
           #:form-error-message
           #:form-field-value
           #:form-field-empty-p
           #:fill-form
           
           #:staticpath
           #:user-theme
           #:skinpath
           #:tmplpath
           #:*rulisp-path*

           #:image

           #:rulisp-start
           ))

(restas:define-module #:rulisp.pcl
  (:use #:cl #:iter #:rulisp.preferences)
  (:export #:pcl-main))

(restas:define-module #:rulisp.jscl
  (:use #:cl #:rulisp.preferences)
  (:export #:jscl-main))

(restas:define-module #:rulisp.auth
  (:use #:cl #:iter)
  (:export #:*sendmail*
           #:*noreply-email*
           #:*re-email-check* 
           #:*finalize-page*
           #:*cookie-auth-name*
           #:*cookie-cipher-key*
           #:*datastore*
           #:*host*))

(restas:define-module #:rulisp.planet
  (:use #:cl #:iter)
  (:export #:*name*
           #:*suggest-mail*
           #:*feeds*
           #:*schedule*
           #:*cache-dir*
           #:*template*))

(restas:define-module #:rulisp.wiki
  (:use #:cl #:iter)
  (:export #:*index-page-title*
           #:*wiki-user-function*

           ;; storage
           #:*storage*
           #:file-storage
           #:storage-find-page
           #:storage-save-page
           #:storage-page-history

           ;; drawer
           #:drawer
           #:finalize-page
           #:render-route-data
           #:generate-content-from-markup)) 

(restas:define-module #:rulisp.colorize
  (:use #:cl #:iter)
  (:export #:*storage*
           #:*finalize-page*
           #:*colorize-user-function*
           #:*max-on-page*
           ;; note
           #:note
           #:note-id
           #:note-title
           #:note-author
           #:note-code
           #:note-date
           #:note-lang
           ;; storage
           #:storage-count-notes
           #:storage-list-notes
           #:storage-get-note
           #:storage-add-note
           #:storage-remove-note
           ;;
           #:finalize-page
           #:render-route-data
           #:colorize
           #:colorize-langs))


(restas:define-module #:rulisp.forum
  (:use #:cl #:iter)
  (:export #:*storage*
           #:*site-name*
           #:*finalize-page*
           #:*max-topic-on-page*
           #:*max-reply-on-page*
           #:*user-name-function*
           #:*rss-item-count*

           ;; storage interface
           #:storage-admin-p
           #:storage-list-forums
           #:storage-list-topics
           #:storage-create-topic
           #:storage-delete-topic
           #:storage-forum-info
           #:storage-topic-message
           #:storage-topic-reply-count
           #:storage-topic-replies
           #:storage-create-reply
           #:storage-delete-reply
           #:storage-reply-position
           #:storage-all-news
           #:storage-forum-news
           #:storage-topic-news))

(defpackage #:wiki-parser
  (:use #:cl #:iter)
  (:export #:parse
           #:remake-lexer
           #:define-parser
           #:define-toplevel-mode
           #:define-mode
           #:bad-element-condition
           #:init-parser))
