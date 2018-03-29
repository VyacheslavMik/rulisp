;;;; forum.lisp
;;;;
;;;; This file is part of the restas-forum library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp.forum)

;;; aux

(defun user-name ()
  (if *user-name-function*
      (funcall *user-name-function*)))

(defun parse-start ()
  (or (ignore-errors (parse-integer (hunchentoot:get-parameter "start")))
      0))

(defun forum-info-plist (info)
  (list :title (second info)
        :href (restas:genurl 'list-topics
                             :forum-id (first info))))

(defun topic-last-page-id (topic-id &optional topic-reply-count)
  (ceiling (or topic-reply-count
               (storage-topic-reply-count *storage* topic-id))
           *max-reply-on-page*))

(defun topic-pages (topic-id current &key topic-reply-count )
  (cons (list :number 1
              :href (restas:genurl 'view-topic
                                   :topic-id topic-id)
              :current (= 1 current))
        (iter (for i from 2 to (topic-last-page-id topic-id topic-reply-count))
              (collect (list :number i
                             :href (restas:genurl 'view-topic-page
                                                  :topic-id topic-id
                                                  :page-id i)
                             :current (= i current))))))

(defun site-name ()
  (or *site-name*
      (if (boundp 'hunchentoot:*request*)
          (hunchentoot:host))
      "RULISP-FORUMS"))

(defun js-urls ()
  (iter (for item in '("jquery.js" "jquery.wysiwyg.js" "jqModal.js" "forum.js"))
        (collect (restas:genurl 'resources.route
                                :path (list "js" item)))))

(defun colorize-traits ()
  (list :href (restas:genurl 'colorize-code)
        :langs (iter (for (id . title) in (colorize:coloring-types))
                     (collect (list :id (symbol-name id)
                                    :title title)))))

;;;; list all forums

(restas:define-route list-forums ("")
  (list :forums (iter (for forum in (storage-list-forums *storage*))
                      (collect (forum-info-plist forum)))
        :feed-href (restas:genurl 'all-forums-rss)
        :title "Все форумы"))

;;;; view forum topics

(restas:define-route list-topics (":forum-id")
  (bind:bind ((start (parse-start))
              ((title total-count) (storage-forum-info *storage* forum-id))
              (href (restas:genurl 'list-topics :forum-id forum-id))
              (adminp (storage-admin-p *storage* (user-name))))
    (flet ((self-url (start)
             (format nil "~A?start=~A" href start)))
      (print (js-urls))
      (list :title title
            :js (js-urls)
            :css '("jquery.wysiwyg.css")
            :href-rss (restas:genurl 'forum-rss :forum-id forum-id)
            :total-count total-count
            :list-forums-href (restas:genurl 'list-forums)
            :href-before (if (< (+ (1- start) *max-topic-on-page*)
                                total-count)
                             (self-url (+ start *max-topic-on-page*)))
            :href-after (if (> start 0)
                            (self-url (max (- start *max-topic-on-page*) 0)))
            :topics (iter (for topic in (storage-list-topics *storage* forum-id *max-topic-on-page* start))
                          (collect (list* :href (restas:genurl 'view-topic
                                                               :topic-id (getf topic :id))
                                          :pages (topic-pages (getf topic :id)
                                                              -1
                                                              :topic-reply-count (getf topic :message-count))
                                          :href-delete (if adminp
                                                           (restas:genurl 'delete-topic
                                                                          :topic-id (getf topic :id)))
                                          topic)))
            :first (1+ start)
            :can-create-new-topic (user-name)
            :colorize (colorize-traits)))))

;;;; create new topic

(restas:define-route create-topic (":forum-id" :method :post)
  (:requirement 'user-name)
  (let ((title (hunchentoot:post-parameter "title"))
        (body (hunchentoot:post-parameter "body")))
    (unless (or (string= title "")
                (string= body ""))
      (storage-create-topic *storage*
                            forum-id
                            title
                            body
                            (user-name)))
    (restas:redirect 'list-topics :forum-id forum-id)))


;;;; delete topic

(restas:define-route delete-topic ("thread/delete/:topic-id")
  (:requirement 'user-name)
  (:sift-variables (topic-id 'integer))
  (if (storage-admin-p *storage* (user-name))
      (restas:redirect 'list-topics
                       :forum-id (storage-delete-topic *storage* topic-id))
      hunchentoot:+http-forbidden+))

;;;; view-topic-page

  
(restas:define-route view-topic-page ("thread/:topic-id/page:(page-id)")
  (:sift-variables (topic-id 'integer) (page-id 'integer))
  (let* ((message (storage-topic-message *storage* topic-id))
         (origin-message-id (getf message :message-id))
         (start (max (* *max-reply-on-page*
                        (1- page-id))
                     0))
         (user (user-name))
         (adminp (if user (storage-admin-p *storage* user))))
    (list :list-forums-href (restas:genurl 'list-forums)
          :js (js-urls)
          :rss-href (restas:genurl 'topic-rss
                                   :topic-id topic-id)
          :parent-forum (forum-info-plist (getf message :forum))
          :message (list* :href-reply (restas:genurl 'create-reply
                                                     :message-id origin-message-id)
                          message)
          :pages (topic-pages topic-id page-id)
          :replies (iter (for item in (storage-topic-replies *storage*
                                                             topic-id
                                                             *max-reply-on-page*
                                                             start))
                         (for reply-id = (getf item :id))
                         (collect (list* :href-delete (if adminp
                                                          (restas:genurl 'delete-message
                                                                         :reply-id reply-id))
                                         :href (restas:genurl 'view-reply
                                                              :reply-id reply-id)
                                         :href-reply (restas:genurl 'create-reply
                                                                    :message-id reply-id)
                                         :prev-msg (let ((prev-id (getf item :prev-id)))
                                                     (if (and (not (eql prev-id :null))
                                                              (not (eql prev-id origin-message-id)))
                                                         (list :author (getf item :prev-author)
                                                               :created (getf item :prev-created)
                                                               :href (restas:genurl 'view-reply
                                                                                    :reply-id (getf item :prev-id)))))
                                         item)))
          :can-create-message user
          :title (getf message :title)
          :colorize (colorize-traits))))

;;;; view topic

(restas:define-route view-topic ("thread/:topic-id")
  (:sift-variables (topic-id 'integer))
  (view-topic-page topic-id 1))


;;;; view-reply

(restas:define-route view-reply ("messages/:reply-id")
  (:sift-variables (:reply-id 'integer))
  (multiple-value-bind (pos topic-id) (storage-reply-position *storage* reply-id)
    (unless topic-id
      (return-from view-reply hunchentoot:+http-not-found+))
    (let ((page (ceiling pos
                         *max-reply-on-page*)))
      (hunchentoot:redirect
       (format nil
               "~A#comment-~A"
               (if (= page 1)
                   (restas:genurl 'view-topic
                                  :topic-id topic-id)
                   (restas:genurl 'view-topic-page
                                  :topic-id topic-id
                                  :page-id page))
               reply-id)))))

;;;; create reply on message

(restas:define-route create-reply ("messages/reply/:message-id")
  (:sift-variables (:message-id 'integer))
  (:requirement 'user-name)
  (declare (ignore message-id))
  )

(restas:define-route create-reply/post ("messages/reply/:message-id" :method :post)
  (:sift-variables (message-id 'integer))
  (:requirement 'user-name)
  (let ((body (hunchentoot:post-parameter "body")))
    (when (string= body "")
      (view-reply message-id))
    (view-reply (storage-create-reply *storage*
                                      message-id
                                      body
                                      (user-name)))))

;;;; delete reply

(restas:define-route delete-message ("message/delete/:(reply-id)")
  (:requirement 'user-name)
  (if (storage-admin-p *storage* (user-name))
      (restas:redirect 'view-topic
                       :topic-id (storage-delete-reply *storage* reply-id))
      hunchentoot:+http-forbidden+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-rss-items (items)
  (iter (for item in items)
        (collect (list* :href (restas:genurl* 'view-reply :reply-id (getf item :id))
                        item))))

(defun make-rss-feed (feed)
  (let ((title (getf feed :title))
        (description (getf feed :description))
        (link (getf feed :link))
        (messages (getf feed :messages)))
    #|------------------------------------------------------------------------|#
    (xtree:with-parse-document(doc  "<rss version=\"2.0\" />")
      #|----------------------------------------------------------------------|#
      (let ((channel (xtree:make-child-element (xtree:root doc) "channel")))
        #|--------------------------------------------------------------------|#
        (setf (xtree:text-content (xtree:make-child-element channel "title")) title
              (xtree:text-content (xtree:make-child-element channel "link")) link
              (xtree:text-content (xtree:make-child-element channel "description")) description)
        #|--------------------------------------------------------------------|#
        (iter (for message in messages)
              (let ((item (xtree:make-child-element channel "item")))
                (setf (xtree:text-content (xtree:make-child-element item "title")) (xtree:encode-special-chars doc (format nil "~A: ~A" (getf message :author) (getf message :title)))
                      (xtree:text-content (xtree:make-child-element item "link")) (xtree:encode-special-chars doc (getf message :href))
                      (xtree:text-content (xtree:make-child-element item "description")) (xtree:encode-special-chars doc (getf message :message))
                      (xtree:text-content (xtree:make-child-element item "pubDate")) (xtree:encode-special-chars doc (getf message :date))))))
      #|----------------------------------------------------------------------|#
      (xtree:serialize doc :to-string))))

(restas:define-route all-forums-rss ("rss/all.rss" :content-type "application/rss+xml")
  (:render-method 'make-rss-feed)
  #|--------------------------------------------------------------------------|#
  (let ((title (format nil "~A: Форумы" (site-name))))
    (list :title title
          :description title
          :link (restas:genurl* 'list-forums)
          :messages (make-rss-items (storage-all-news *storage* *rss-item-count*)))))
                        
(restas:define-route forum-rss ("rss/:(forum-id).rss" :content-type "application/rss+xml")
  (:render-method 'make-rss-feed)
  #|--------------------------------------------------------------------------|#
  (let ((title (format nil
                       "~A: Форум - ~A"
                       (site-name)
                       (first (storage-forum-info *storage* forum-id)))))
    (list :title title
          :description title
          :link (restas:genurl* 'list-topics :forum-id forum-id)
          :messages (make-rss-items (storage-forum-news *storage* forum-id *rss-item-count*)))))

(restas:define-route topic-rss ("rss/threads/:(topic-id).rss" :content-type "application/rss+xml")
  (:sift-variables (topic-id 'integer))
  (:render-method 'make-rss-feed)
  #|--------------------------------------------------------------------------|#
  (let ((message (storage-topic-message *storage* topic-id)))
    (list :title (format nil
                         "~A: ~A"
                         (site-name)
                         (getf message :title))
          :description (getf message :body)
          :link (restas:genurl* 'view-topic :topic-id topic-id)
          :messages (make-rss-items (storage-topic-news *storage* topic-id *rss-item-count*)))))
                                       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Colorize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route colorize-code ("colorize" :method :post)
  (:render-method 'identity)
  (let ((code (hunchentoot:post-parameter "code"))
        (lang (hunchentoot:post-parameter "lang")))
    (colorize::html-colorization (or (find-symbol lang :keyword)
                                     (error "Unknow coloring type: ~A" lang))
                                 code)))

  
