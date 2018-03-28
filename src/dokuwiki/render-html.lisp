;;;; render-html.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-format-aux (format args)
  (if (symbolp format)
      (apply #'restas:genurl format args)
      (if args
          (apply #'format nil (cons format args))
          format)))

(defun eid (format &rest args)
  "Make id attribute"
  (xfactory:attributes :id
                       (apply-format-aux format args)))

(defun eclass (format &rest args)
  "Make class attribute"
  (xfactory:attributes :class
                       (apply-format-aux format args)))

(defun ehref (format &rest args)
  "Make href attribute"
  (xfactory:attributes :href
                       (apply-format-aux format args)))

(defun estyle (format &rest args)
  "Make style attributes"
  (xfactory:attributes :style
                       (apply-format-aux format args)))

(defun escript (src &optional (type "text/javascript"))
  "Make script element"
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "script")))
    (xfactory:attributes :src src
                         :type type)))

(defun ecss (format &rest args)
  "Make link css element"
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "link")))
    (xfactory:attributes :href (apply-format-aux format args)
                         :rel "stylesheet"
                         :type "text/css")))

(defun e-break-line ()
  "Make br element"
  (xtree:make-child-element xfactory:*node* "br"))

(defun estrong (format &rest args)
  "Make strong element"
  (xtree:make-child-text (xtree:make-child-element xfactory:*node*
                                          "strong")
                         (apply-format-aux format args)))

(defun e-text2html (text)
  "parse text as html and append to current element"
  (if text
      (html:with-parse-html (html text)
        (when html
          (iter (for node in (iter (for node in-child-nodes (xpath:find-single-node html "/html/body"))
                                   (collect node)))
                (xtree:detach node)
                (xtree:append-child xfactory:*node* node))))))

(defun etext (format &rest args)
  (apply #'xfactory:text
         format
         args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *wiki-render-map* (make-hash-table))

(defun todo-dokuwiki ()
  (set-difference (iter (for (key value) in-hashtable dokuwiki::*symbols-category*)
                        (collect key))
                  (iter (for (key value) in-hashtable *wiki-render-map*)
                        (collect key))))

(defun render-wiki-item (item)
  (cond
    ((and (consp item)
          (symbolp (car item))) (let ((render (gethash (car item) *wiki-render-map*)))
                                  (if render
                                      (funcall render (cdr item))
                                      (render-wiki-item (cdr item)))))
    ((consp item) (iter (for i in item)
                        (render-wiki-item i)))
    ((symbolp item) (let ((render (gethash item *wiki-render-map*)))
                      (if render
                          (funcall render nil))))
    ((stringp item) (xfactory:text item))))

(defvar *footnotes*)
(defvar *footnote-number*)

(defun make-wiki-toc (wikidoc)
  (iter (for item in wikidoc)
        (when (and (consp item)
                   (eql (car item) 'dokuwiki:chapter))
          (let* ((suppose-ul (xtree:last-child xfactory:*node*))
                 (ul (if (and suppose-ul
                              (string= (xtree:local-name suppose-ul) "ul"))
                         suppose-ul
                         (xtree:make-child-element xfactory:*node*
                                                   "ul")))                 
                 (xfactory:*node* (xtree:make-child-element ul "li"))
                 (name (second (second item))))
            (xfactory:with-element-factory ((E))
              (E :div
                 (E :a
                    (ehref "#~A" name)
                    (etext name))
                 (make-wiki-toc (cddr item))))))))

(define-condition notoc-condition (error) ())

(defun total-chapters-count (wikidoc)
  (cond
    ((eql wikidoc 'dokuwiki:notoc) (error 'notoc-condition))
    ((consp wikidoc) (iter (for item in wikidoc)
                           (sum (total-chapters-count item))))
    ((eql wikidoc 'dokuwiki:chapter) 1)
    (t 0)))

(defun render-wiki-page (wikidoc)
  (let ((*footnotes* (xtree:make-element "div"))
        (*footnote-number* 0))
    (xfactory:with-element-factory ((E))
      (E :div
         (eclass "article")
         (handler-case 
             (when (> (total-chapters-count wikidoc) 2)
               (E :div
                  (eclass "toc")
                  (E :div
                     (eclass "toc-header")
                     "Содержание")
                  (E :div
                     (eclass "toc-body")
                     (make-wiki-toc wikidoc))))
           (notoc-condition ()))
         (render-wiki-item wikidoc)
         (if (xtree:first-child *footnotes*)
             (progn
               (setf (xtree:attribute-value *footnotes* "class") "footnotes")
               (xtree:append-child xfactory:*node* *footnotes*))
             (xtree:release *footnotes*))))))

(defun render-wiki-page-to-string (wikidoc)
  (xtree:with-object (html (render-wiki-page wikidoc))
    (xtree:serialize html :to-string)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render-all-wiki-items (items)
  (iter (for item in items)
        (render-wiki-item item)))


(defmacro define-wiki-render (name (items) &body body)
  `(setf (gethash ',name
                  *wiki-render-map*)
         (lambda (,items)
           ,@body)))

(define-wiki-render dokuwiki:toplevel (items)
  (render-all-wiki-items items))
  
(define-wiki-render dokuwiki:chapter (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node*
                                                   "div")))
    (eclass "chapter")
    (eid (second (first items))) 
    (render-all-wiki-items items)))


(define-wiki-render dokuwiki:header (items)
  (xtree:make-child-text (xtree:make-child-element xfactory:*node*
                                                   "h3")
                         (first items)))

(defparameter +endl+ (string #\Newline))

(define-wiki-render dokuwiki:eol (items)
  (declare (ignore items))
  (xfactory:text +endl+))


(define-wiki-render dokuwiki:paragraph (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node*
                                                   "p")))
    (render-all-wiki-items items)))


(define-wiki-render dokuwiki:footnote (items)
  (incf *footnote-number*)
  (xfactory:with-element-factory ((E))
    (E :a
       (eclass "fn_top")
       (eid "fnt__~A" *footnote-number*)
       (ehref "#fn__~A" *footnote-number*)
       (xfactory:text "~A)" *footnote-number*)))
  (let ((xfactory:*node* *footnotes*))
    (xfactory:with-element-factory ((E))
      (E :div
         (E :a
            (eclass "fn_bot")
            (eid "fn__~A" *footnote-number*)
            (ehref "#fnt__~A" *footnote-number*)
            (xfactory:text "~A)" *footnote-number*))
         (render-all-wiki-items items)))))

(define-wiki-render dokuwiki:linebreak (items)
  (declare (ignore items))
  (xtree:make-child-element xfactory:*node* "br"))

(define-wiki-render dokuwiki:monospace (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "code")))
    (render-all-wiki-items items)))

(define-wiki-render dokuwiki:strong (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "strong")))
    (render-all-wiki-items items)))

(define-wiki-render dokuwiki:emphasis (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "em")))
    (render-all-wiki-items items)))

(define-wiki-render dokuwiki:underline (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "span")))
    (eclass "underline")
    (render-all-wiki-items items)))

(define-wiki-render dokuwiki:preformatted (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "pre")))
    (iter (for item in items)
          (render-wiki-item item)
          (e-break-line))))
  
(defun code-to-html (code)
  (flet ((empty-line-p (line)
           (string= (string-trim #(#\Space #\Tab) line) "")))
    (let ((lines (split-sequence:split-sequence #\Newline code)))
      (iter
        (while lines)
        (for isempty = (empty-line-p (first lines)))
        (if isempty
            (setf lines (cdr lines)))
        (while isempty))
      (iter
        (while lines)
        (for isempty = (empty-line-p (car (last lines))))
        (if isempty
            (setf lines
                  (remove (car (last lines)) lines)))
        (while isempty))
      (let ((min-space-count (iter (for line in (remove-if #'empty-line-p lines))
                                   (minimize (or (position #\Space line :test-not #'char-equal) 0)))))
        (setf lines
              (iter (for line in lines)
                    (collect (if (empty-line-p line)
                                 ""
                                 (subseq line min-space-count))))))
      (colorize::html-colorization :common-lisp
                                   (format nil "~{~A~%~}" lines)))))

(define-wiki-render dokuwiki:code (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "pre")))
    (eclass "code")
    (e-text2html (code-to-html (car items)))))

(define-wiki-render dokuwiki:quoted (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "blockquote")))
    (render-all-wiki-items items)))

(define-wiki-render dokuwiki:unformatted (items)
  (iter (for item in (alexandria:flatten items))
        (cond
          ((stringp item) (xfactory:text item))
          ((eql item 'dokuwiki:eol) (xfactory:text +endl+)))))

(define-wiki-render dokuwiki:unformattedalt (items)
  (iter (for item in (alexandria:flatten items))
        (cond
          ((stringp item) (xfactory:text item))
          ((eql item 'dokuwiki:eol) (xfactory:text +endl+)))))

(define-wiki-render dokuwiki:html (items)
  (xfactory:with-element-factory ((E))
    (E :pre
       (iter (for item in (alexandria:flatten items))
        (cond
          ((stringp item) (xfactory:text item))
          ((eql item 'dokuwiki:eol) (xfactory:text +endl+)))))))
       

;;(define-wiki-render dokuwiki:html (items)

(define-wiki-render dokuwiki:hr (items)
  (declare (ignore items))
  (xtree:make-child-element xfactory:*node* "hr"))

(define-wiki-render dokuwiki:unordered-listblock (items)
  (xfactory:with-element-factory ((E))
    (E :ul
       (iter (for item in items)
             (E :li
                (render-wiki-item item))))))

(define-wiki-render dokuwiki:ordered-listblock (items)
  (xfactory:with-element-factory ((E))
    (E :ol
       (iter (for item in items)
             (E :li
                (render-wiki-item item))))))

(defconstant +EN-DASH+
  #+sbcl #\EN_DASH
  #+ccl #\U+2013)

(define-wiki-render dokuwiki:en-dash (items)
  (declare (ignore items))
  (xfactory:text (string +EN-DASH+)))

(defconstant +EM-DASH+
  #+sbcl #\EM_DASH
  #+ccl #\U+2014)

(define-wiki-render dokuwiki:em-dash (items)
  (declare (ignore items))
  (xfactory:text (string +EM-DASH+)))

(define-wiki-render dokuwiki:internal-link (items)
  (let ((delimiter (position #\| (car items))))
    (xfactory:with-element-factory ((E))
      (E :a
         (ehref "~A"
                (or (ignore-errors
                      (puri:merge-uris (string-trim '#(#\Space #\Tab)
                                                    (if delimiter
                                                        (subseq (car items) 0 delimiter)
                                                        (car items)))
                                       (hunchentoot:request-uri*)))
                    (string-trim '#(#\Space #\Tab)
                                 (if delimiter
                                     (subseq (car items) 0 delimiter)
                                     (car items)))))
         (xfactory:text (string-trim '#(#\Space #\Tab)
                                     (if delimiter
                                         (subseq (car items) (1+ delimiter))
                                         (car items))))))))

(define-wiki-render dokuwiki:media  (items)
  (let ((img (xtree:make-child-element xfactory:*node* "img")))
    (setf (xtree:attribute-value img "alt")
          "Изображение")
    (setf (xtree:attribute-value img "src")
          (string-trim #(#\Space #\Tab #\Newline)
                       (car items)))))


(define-wiki-render dokuwiki:external-link (items)
  (let ((link (xtree:make-child-element xfactory:*node* "a")))
    (setf (xtree:attribute-value link "href") (car items))
    (xtree:make-child-text link (car items))))

(define-wiki-render dokuwiki:table (items)
  (xfactory:with-element-factory ((E))
    (E :table
       (E :tbody
          (iter (for item in items)
                (E :tr
                   (render-all-wiki-items (remove-if-not #'consp item))))))))

(defun render-table-cell (type items)
  (if (or items
          (null (xtree:first-child xfactory:*node*)))
      (let* ((xfactory:*node* (xtree:make-child-element xfactory:*node* type))
             (first (first items))
             (right (and (stringp first)
                         (> (length first) 1)
                         (string= (subseq first 0 2) "  ")))
             (last (car (last items)))
             (left (and (stringp last)
                        (> (length last) 1)
                        (string= (subseq last (- (length last) 2)) "  "))))
        (cond
          ((and right left) (eclass "centeralign"))
          (left (eclass "leftalign"))
          (right (eclass "rightalign")))
        (render-all-wiki-items items))
      (let* ((cell (xtree:last-child xfactory:*node*))
             (colspan (xtree:attribute-value cell "colspan")))
        (setf (xtree:attribute-value cell "colspan")
              (if colspan
                  (write-to-string (1+ (parse-integer colspan)))
                  "2")))))
        

(define-wiki-render dokuwiki:table-header-cell (items)
  (render-table-cell "th" items))

(define-wiki-render dokuwiki:table-cell (items)
  (render-table-cell "td" items))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dokuwiki-drawer (rulisp.wiki:drawer) ())

(defmethod rulisp.wiki:generate-content-from-markup ((drawer dokuwiki-drawer) data)
  (render-wiki-page-to-string (wiki-parser:parse :dokuwiki (call-next-method))))
