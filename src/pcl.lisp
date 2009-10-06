;;; pcl.lisp

(in-package :rulisp.pcl)


(defparameter *pcl-files-map*
  '#(("introduction-why-lisp"
      "Введение: почему Lisp?" 
      "%D0%B2%D0%B2%D0%B5%D0%B4%D0%B5%D0%BD%D0%B8%D0%B5%D0%BF%D0%BE%D1%87%D0%B5%D0%BC%D1%83lisp")
    
     ("lather-rinse-repeat-a-tour-of-the-repl"
      "Намылить, смыть, повторить: знакомство с REPL"
      "%D1%82%D1%83%D1%80%D0%B2repl")
    
     ("practical-a-simple-database"
      "Практикум: Простая база данных"
      "%D0%BF%D1%80%D0%B0%D0%BA%D1%82%D0%B8%D0%BA%D1%83%D0%BC%D0%BF%D1%80%D0%BE%D1%81%D1%82%D0%B0%D1%8F%D0%B1%D0%B0%D0%B7%D0%B0%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85")
    
     ("syntax-and-semantics"
      "Синтаксис и семантика"
      "%D1%81%D0%B8%D0%BD%D1%82%D0%B0%D0%BA%D1%81%D0%B8%D1%81%D0%B8%D1%81%D0%B5%D0%BC%D0%B0%D0%BD%D1%82%D0%B8%D0%BA%D0%B0")
    
     ("functions"
      "Функции"
      "%D1%84%D1%83%D0%BD%D0%BA%D1%86%D0%B8%D0%B8")
    
     ("variables"
      "Переменные"
      "%D0%BF%D0%B5%D1%80%D0%B5%D0%BC%D0%B5%D0%BD%D0%BD%D1%8B%D0%B5")
    
     ("macros-standard-control-constructs"
      "Макросы: Стандартные управляющие конструкции"
      "%D0%BC%D0%B0%D0%BA%D1%80%D0%BE%D1%81%D1%8B%D1%81%D1%82%D0%B0%D0%BD%D0%B4%D0%B0%D1%80%D1%82%D0%BD%D1%8B%D0%B5%D1%83%D0%BF%D1%80%D0%B0%D0%B2%D0%BB%D1%8F%D1%8E%D1%89%D0%B8%D0%B5%D0%BA%D0%BE%D0%BD%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%86%D0%B8%D0%B8")
    
     ("macros-defining-your-own"
      "Макросы: Создание собственных макросов"
      "%D0%BC%D0%B0%D0%BA%D1%80%D0%BE%D1%81%D1%8B%D1%81%D0%BE%D0%B7%D0%B4%D0%B0%D0%BD%D0%B8%D0%B5%D1%81%D0%BE%D0%B1%D1%81%D1%82%D0%B2%D0%B5%D0%BD%D0%BD%D1%8B%D1%85%D0%BC%D0%B0%D0%BA%D1%80%D0%BE%D1%81%D0%BE%D0%B2")
    
     ("practical-building-a-unit-test-framework"
      "Практикум: Каркас для юнит-тестирования"
      "%D0%BF%D1%80%D0%B0%D0%BA%D1%82%D0%B8%D0%BA%D1%83%D0%BC%D0%BA%D0%B0%D1%80%D0%BA%D0%B0%D1%81%D0%B4%D0%BB%D1%8F%D1%8E%D0%BD%D0%B8%D1%82%D1%82%D0%B5%D1%81%D1%82%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F")
    
     ("numbers-characters-and-strings"
      "Числа, знаки и строки"
      "%D1%87%D0%B8%D1%81%D0%BB%D0%B0%D1%81%D0%B8%D0%BC%D0%B2%D0%BE%D0%BB%D1%8B%D1%81%D1%82%D1%80%D0%BE%D0%BA%D0%B8")
    
     ("collections"
      "Коллекции"
      "%D0%BA%D0%BE%D0%BB%D0%BB%D0%B5%D0%BA%D1%86%D0%B8%D0%B8")
    
     ("they-called-it-lisp-for-a-reason-list-processing"
      "Он называется Lisp неспроста: обработка списков"
      "%D0%BE%D0%BD%D0%BD%D0%B0%D0%B7%D1%8B%D0%B2%D0%B0%D0%B5%D1%82%D1%81%D1%8Flisp%D0%BD%D0%B5%D1%81%D0%BF%D1%80%D0%BE%D1%81%D1%82%D0%B0%D0%BE%D0%B1%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%BA%D0%B0%D1%81%D0%BF%D0%B8%D1%81%D0%BA%D0%BE%D0%B2")
    
     ("beyond-lists-other-uses-for-cons-cells"
      "Не только списки: Другие применения cons-ячеек"
      "%D0%BD%D0%B5%D1%82%D0%BE%D0%BB%D1%8C%D0%BA%D0%BE%D1%81%D0%BF%D0%B8%D1%81%D0%BA%D0%B8")
    
     ("files-and-file-io"
      "Файлы и файловый ввод/вывод"
      "%D1%84%D0%B0%D0%B9%D0%BB%D1%8B%D1%84%D0%B0%D0%B9%D0%BB%D0%BE%D0%B2%D1%8B%D0%B9%D0%B2%D0%B2%D0%BE%D0%B4%D0%B2%D1%8B%D0%B2%D0%BE%D0%B4")
    
     ("practical-a-portable-pathname-library"
      "Практика. Переносимая библиотека файловых путей"
      "practicalportablepathlib")
    
     ("object-reorientation-generic-functions"
      "ООП: Обобщенные функции"
      "oopgenericfunctions")
    
     ("object-reorientation-classes"
      "ООП: Классы"
      "oopclasses")
    
     ("a-few-format-recipes"
      "Несколько рецептов для функции FORMAT"
      "%D0%BD%D0%B5%D1%81%D0%BA%D0%BE%D0%BB%D1%8C%D0%BA%D0%BE%D1%80%D0%B5%D1%86%D0%B5%D0%BF%D1%82%D0%BE%D0%B2%D0%B4%D0%BB%D1%8Fformat")
    
     ("beyond-exception-handling-conditions-and-restarts"
      "Обработка исключений изнутри: Условия и Перезапуск"
      "%D0%BE%D0%B1%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%BA%D0%B0%D0%B8%D1%81%D0%BA%D0%BB%D1%8E%D1%87%D0%B5%D0%BD%D0%B8%D0%B9")
    
     ("the-special-operators"
      "Специальные операторы"
      "%D1%81%D0%BF%D0%B5%D1%86%D0%B8%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D0%B5%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80%D1%8B")
    
     ("programming-in-the-large-packages-and-symbols"
      "Программирование по-взрослому: Пакеты и Символы"
      "%D0%BF%D0%B0%D0%BA%D0%B5%D1%82%D1%8B%D0%B8%D1%81%D0%B8%D0%BC%D0%B2%D0%BE%D0%BB%D1%8B")
    
     ("loop-for-black-belts" "LOOP для мастеров с чёрным поясом" "loopforblackbelts")
    
     ("practical-a-spam-filter" "Практика. Спам-фильтр" "practicespamfilter")
    
     ("practical-parsing-binary-files" "Практика. Разбор двоичных файлов" "practiceparsingbinfiles")
    
     ("practical-an-id3-parser" "Практика. Разбор ID3" "practiceanid3parser")
    
     ("practical-web-programming-with-allegroserve" "Практика. Web-программирование с помощью AllegroServe" "practicewebprogramming")
    
     ("practical-an-mp3-database" "Практика. База данных для MP3" "mp3database")
    
     ("practical-a-shoutcast-server" "Практика. Сервер Shoutcast" "shoutcastserver")
    
     ("practical-an-mp3-browser" "Практика. Браузер MP3 файлов" "practicemp3browser")
    
     ("practical-an-html-generation-library-the-interpreter"
      "Практика: Библиотека для генерации HTML, Интерпретатор."
      "practicehtmlgenlibinterpreter")
    
     ("practical-an-html-generation-library-the-compiler"
      "Практика: Библиотека для генерации HTML, Компилятор."
      "practicehtmlgenlibcompiler")
    
     ("conclusion-whats-next" "Заключение: Что дальше ?" "conclusion")))

                               
(defun pcl-source-path (chapter)
  (merge-pathnames (concatenate 'string chapter ".txt")
                   *pcl-dir*))

(define-simple-route pcl-main ("")
  (in-pool
   (xfactory:with-document-factory ((E))
     (E :overlay
        (E :head
           (E :title "Перевод Practical Common Lisp"))
        (E :div
           (eid "content")
           (E :p
              "Это перевод на русский язык замечательной книги "
              (E :a (ehref "http://www.gigamonkeys.com/book/") "Practical Common Lisp")
              ". Основная работа над переводом ведётся "
              (E :a (ehref "http://pcl.catap.ru/") "здесь")
              ". "              
              (e-break-line)
              (estrong "ОСТОРОЖНО!")
              " Этот сервис основан на ещё не отлаженном коде по парсингу и отображению "
              (E :a (ehref "http://www.dokuwiki.org/ru:dokuwiki") "dokuwiki")
              "-страниц,
 если Вы хотите быть уверены в точности отображения содержимого - обратитесь к "
              (E :a (ehref "http://pcl.catap.ru/") "источнику")
              " перевода")
           (E :p
              (E :a
                 (ehref 'pcl-pdf)
                 (eclass "pdf-link")
                 "PDF-версия"))
           (E :img
              (xfactory:attributes :src (restas:genurl 'rulisp.static::image :file "pcl.jpg")
                                   :alt "PCL"
                                   :style "float: right"))
              
           (E :ol
              (iter (for chapter in-vector *pcl-files-map*)
                    (E :li
                       (E :a
                          (ehref 'pcl-chapter-view :chapter (first chapter))
                          (xfactory:text (second chapter)))))))))))

(defun pcl-navigation-bar (number)
  (xfactory:with-element-factory ((E))
    (E :table
       (xfactory:attributes :width "100%")
       (E :tbody
          (E :tr
             (E :td
                (xfactory:attributes :width "20%"
                                     :align "left")
                (when (> number 0)
                  (E :a
                     (ehref 'pcl-chapter-view
                            :chapter (first (aref *pcl-files-map*
                                                  (1- number))))
                     "Предыдущая")))
             (E :td 
                (xfactory:attributes :width "60%"
                                     :align "center")
                (E :a
                   (ehref 'pcl-main)
                   "Оглавление"))
             (E :td
                (xfactory:attributes :width "20%"
                                     :align "right")
                (when (< number (1- (length *pcl-files-map*)))
                  (E :a
                     (ehref 'pcl-chapter-view
                            :chapter (first (aref *pcl-files-map*
                                                  (1+ number))))
                     "Следующая"))))))))

(define-simple-route pcl-chapter-view (":(chapter)")
  (let* ((number (position chapter
                           *pcl-files-map*
                           :key #'first
                           :test #'string=))
         (path (pcl-source-path (third (aref *pcl-files-map* number)))))
    (if (fad:file-exists-p path)
        (in-pool
         (xfactory:with-document-factory ((E))
           (E :overlay
              (E :head
                 (E :title
                    (xfactory:text "~A" (second (aref *pcl-files-map* number)))))
              (E :div
                 (eid "content")
                 (pcl-navigation-bar number)
                 (rulisp.wiki::render-wiki-page (wiki-parser:parse :dokuwiki
                                                      path))
                 (pcl-navigation-bar number)))))
        hunchentoot:+HTTP-NOT-FOUND+)))


(define-simple-route pcl-chapter-pdf ("pdf/:(chapter)"
                                      :content-type "application/pdf")
  (let* ((number (position chapter
                           *pcl-files-map*
                           :key #'first
                           :test #'string=))
         (path (pcl-source-path (third (aref *pcl-files-map* number)))))
    (flexi-streams:with-output-to-sequence (out)
      (let ((out* (flexi-streams:make-flexi-stream out)))
        (rulisp.wiki::pdf-render-wiki-page (wiki-parser:parse :dokuwiki
                                                 path)
                              out*))
      out)))


(defun pcl-first-page ()
  (let ((result))
    (pdf:with-page ()
      (setf result pdf:*page*)
      ;;(pdf:draw-centered-text 300 500 "Practical Common Lisp" *header-font* 30)
      (let ((bounds (pdf::bounds pdf:*page*))
            (image (pdf:make-image (staticpath "image/pcl.jpg"))))
        (pdf:add-images-to-page image)
        (pdf:draw-image image
                        0 0 (aref bounds 2) (aref bounds 3) 0))
      )
    result))


(defun make-pcl-pdf (&optional (out #P"/tmp/pcl.pdf"))
  (let ((page-number 1))
    (tt:with-document (:mode :outlines)
      (pdf:append-child-outline (pdf:outline-root pdf:*document*) 
                                "Practical Common Lisp"
                                (pdf:register-reference :name "Practical Common Lisp"
                                                        :page (pcl-first-page)))
      (let ((*current-chapter* "Practical Common Lisp"))
        (iter (for chapter in-vector *pcl-files-map*)
              (for i from 1)
              (print i)
              (let ((wikidoc (wiki-parser:parse :dokuwiki
                                                (pcl-source-path (third chapter)))))
                (tt:draw-pages 
                 (tt:compile-text ()
                   (tt:with-style (:font *base-font* :font-size *font-size*)       
                     (rulisp.wiki::pdf-render-wiki-item wikidoc)))
                 :break :after
                 :margins '(30 50 30 40)
                 :finalize-fn #'(lambda (page)
                                  (pdf:draw-centered-text (/ (aref (pdf::bounds page) 2) 2)
                                                          10
                                                          (write-to-string (incf page-number))
                                                          *base-font*
                                                          10)
                                  )))
              (pdf:write-document out))))))

(define-simple-route pcl-pdf ("pcl.pdf")
  (merge-pathnames "pcl.pdf"
                   *pcl-snapshot-dir*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load snapshot from http://pcl.catap.ru/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-pcl-snapshot ()
  (let ((snapshot-path (ensure-directories-exist (merge-pathnames (car (last (puri:uri-parsed-path *pcl-snapshot-url*)))
                                                                  *pcl-snapshot-dir*)))
        (snapshot (drakma:http-request *pcl-snapshot-url*
                                       :force-binary t)))
    (when snapshot
      (with-open-file (out
                       snapshot-path
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
        (write-sequence snapshot out))
      (zip:unzip snapshot-path
                 *pcl-snapshot-dir*
                 :if-exists :supersede)
      (setf *pcl-dir*
            (merge-pathnames "var/www/pcl.catap.ru/htdocs/data/pages/pcl/"
                             *pcl-snapshot-dir*))
        
      (make-pcl-pdf (merge-pathnames "pcl.pdf.tmp"
                                     *pcl-snapshot-dir*))
      (sb-posix:rename (merge-pathnames "pcl.pdf.tmp"
                                        *pcl-snapshot-dir*)
                       (merge-pathnames "pcl.pdf"
                                        *pcl-snapshot-dir*))
      t)))

(if *pcl-load-snapshot-p*
    (clon:schedule-function 'load-pcl-snapshot
                            (clon:make-scheduler (clon:make-typed-cron-schedule :hour '*)
                                                 :allow-now-p t)
                            :thread t))