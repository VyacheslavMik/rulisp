;;;; sendmail.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp.auth)

(defun prepare-subject (subject &optional (external-format :utf-8))
  (format nil
          "=?~A?B?~A?="
          external-format
          (base64:string-to-base64-string
           (coerce (loop for code across (babel:string-to-octets subject
                                                                 :encoding external-format)
                      collect (code-char code))
                   'string))))

(defun send-mail (to content)
  #+sbcl(let* ((sendmail-process (sb-ext:run-program *sendmail*
                                                     to
                                                     :input :stream
                                                     :output nil
                                                     :error nil
                                                     :wait nil))
               (sendmail (sb-ext:process-input sendmail-process)))
          (unwind-protect
               (write-string content sendmail)
            (close sendmail)
            (sb-ext:process-wait sendmail-process)
            (sb-ext:process-close sendmail-process))))
