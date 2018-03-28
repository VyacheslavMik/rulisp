;;;; storage.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp.wiki)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; storage generic interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric storage-find-page (storage title)
  (:documentation "Find wiki page in storage"))

(defgeneric storage-save-page (storage title content author &optional comment)
  (:documentation "Save wiki page in storage"))

(defgeneric storage-page-history (storage title)
  (:documentation "Return page history"))

(defgeneric storage-page-version (storage title version)
  (:documentation "Find wiki page version in storage"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; file storage 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-string-into-gzip-file (string path)
  (with-open-file (ostream
                   path
                   :element-type '(unsigned-byte 8)
                   :direction :output
                   :if-exists :supersede)
    (salza2:with-compressor (compressor 'salza2:gzip-compressor
                                        :callback (salza2:make-stream-output-callback ostream))
      (salza2:compress-octet-vector (babel:string-to-octets string :encoding :utf-8)  
                                    compressor))))

(defun read-gzip-file-into-string (path)
  (babel:octets-to-string (with-open-file (in path :element-type '(unsigned-byte 8))
                            (zip:skip-gzip-header in)
                            (flex:with-output-to-sequence (out)
                              (zip:inflate in out)))
                          :encoding :utf-8))


(defclass file-storage ()
  ((dir :initarg :dir :reader file-storage-dir)))

(defun encode-page-title (title)
  (closure-template:encode-uri title))

(defun file-storage-page-pathname (storage title)
  (merge-pathnames (format nil "pages/~A" (encode-page-title title))
                   (file-storage-dir storage)))

(defun file-storage-changes-pathname (storage title)
  (merge-pathnames (format nil "changes/~A.changes" (encode-page-title title))
                   (file-storage-dir storage)))

(defun file-storage-archive-pathname (storage page time)
  (merge-pathnames (format nil "archive/~A.~A.gz" (encode-page-title page) time)
                   (file-storage-dir storage)))

(defmethod storage-find-page ((storage file-storage) title)
  (let ((path (file-storage-page-pathname storage title)))
    (if (fad:file-exists-p path)
        (alexandria:read-file-into-string path))))

(defmethod storage-save-page ((storage file-storage) title content author &optional comment)
  (let* ((time (get-universal-time))
         (page-path (ensure-directories-exist (file-storage-page-pathname storage title)))
         (changes-path (ensure-directories-exist (file-storage-changes-pathname storage title)))
         (archive-path (ensure-directories-exist (file-storage-archive-pathname storage title time)))
         (changes (nconc (if (fad:file-exists-p changes-path)
                             (with-open-file (in changes-path)
                               (with-standard-io-syntax
                                 (read in))))
                         (list (list time
                                     author
                                     (if (fad:file-exists-p page-path)
                                         :edit
                                         :create)
                                     title
                                     comment)))))
    (with-open-file (out changes-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-standard-io-syntax
        (print changes
               out)))
    (write-string-into-gzip-file content archive-path)
    (alexandria:write-string-into-file content
                                       page-path
                                       :if-exists :supersede
                                       :if-does-not-exist :create)))

(defmethod storage-page-history ((storage file-storage) title)
  (let ((path (file-storage-changes-pathname storage title)))
    (when (fad:file-exists-p path)
      (iter (for item in (nreverse (with-open-file (in path)
                                     (with-standard-io-syntax
                                       (read in)))))
            (collect (list :date (first item)
                           :name (fourth item)
                           :author (second item)))))))
            
(defmethod storage-page-version ((storage file-storage) title version)
  (let ((path (file-storage-archive-pathname storage title version)))
    (if (fad:file-exists-p path)
        (read-gzip-file-into-string path))))
