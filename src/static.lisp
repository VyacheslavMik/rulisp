;;;; static.lisp

(in-package :rulisp.static)

(defparameter *rulisp-ns* "chrome://rulisp/")

(xpath:define-xpath-function colorize (code)
  (code-to-html code))

(xslt:define-xslt-element text2html (self input output)
  (let ((text (xpath:find-string input
                                 (xtree:attribute-value self "select"))))
    (if text
        (html:with-parse-html (doc text)
          (let ((root (or (xpath:find-single-node (xtree:root doc) "body")
                                             (xtree:root doc))))
          (iter (for node in-child-nodes root)
                (xtree:append-child output (xtree:copy node))))))))

(xslt:defxsl *content-xsl* (merge-pathnames "src/xsl/content.xsl" *rulisp-path*))
(xslt:defxsl *articles-xsl* (merge-pathnames "src/xsl/articles.xsl" *rulisp-path*))

(defun apply-xsl (style obj)
  (let ((xpath:*lisp-xpath-functions* `((colorize "colorize" ,*rulisp-ns*)))
        (xslt:*lisp-xslt-elements* `((text2html "text2html" ,*rulisp-ns*)))r
        (path (merge-pathnames obj *rulisp-path*)))
    (if (fad:file-exists-p path)
        (in-pool (xslt:transform style
                                         (in-pool (xtree:parse path :xml-parse-noent ))))
        hunchentoot:+HTTP-NOT-FOUND+)))

(define-simple-route main ("")
  (apply-xsl *content-xsl* "content/index.xml"))


(define-simple-route css ("/css/:(theme)/:(file)")
  (skinpath (format nil "css/~A" file)
            theme))

(define-simple-route image ("image/:(file)")
  (staticpath (format nil "image/~A" file)))

(define-simple-route js ("js/:(file)")
  (staticpath (format nil "js/~A" file)))

(define-simple-route articles ("articles/")
  (apply-xsl *content-xsl*
             "content/articles/index.xml"))

(define-simple-route article ("articles/:(afile)")
  (let ((afile-length (length afile)))
    (if (and (> afile-length 4)
             (string= (subseq afile (- afile-length 5))
                      ".html"))
        (redirect 'article :afile (subseq afile 0 (- afile-length 5)))
        (apply-xsl *articles-xsl*
                   (format nil "content/articles/~A.xml" afile)))))

(define-simple-route favicon ("favicon.ico")
  (staticpath "favicon.ico"))

(define-simple-route tools-list ("apps/")
  (in-pool (xtree:parse (tmplpath "apps.xml"))))



;; (defparameter *mainmenu* '((main "Главная")                           
;;                            (articles "Статьи")
;;                            (rulisp.planet::planet-main "Планета")
;;                            (rulisp.forum::forum-main "Форум")                           
;;                            (tools-list "Сервисы")
;;                            (rulisp.pcl::pcl-main "Practical Common Lisp")
;;                            (rulisp.wiki::wiki-main-page "wiki")))

(defparameter *mainmenu* nil)

(define-simple-route mainmenu ("mainmenu"
                               :protocol :chrome)
  (in-pool
   (xfactory:with-document-factory ((E))
     (E :ul
        (iter (for (plugin route name) in *mainmenu*)
              (E :li
                 (E :a
                    (xfactory:attributes :href
                                         (restas:site-url plugin route))
                    (xfactory:text name))))))))


(define-simple-route theme-css-include ("theme/css/:(file)"
                                        :protocol :chrome)
  (format nil
          "<link href=\"~A\" rel=\"stylesheet\" type=\"text/css\" />"
          (genurl 'css :theme (user-theme (username)) :file file)))
  