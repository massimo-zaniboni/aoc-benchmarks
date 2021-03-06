
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; Copyright (C) 2021 Massimo Zaniboni <mzan@dokmelody.org>

;; WARNING: I'm learning CL

(ql:quickload :trivia)     ;; common macro and functions and optimal pattern matching
(ql:quickload :alexandria) ;; common CL extensions
(ql:quickload :trivial-types)  ;; common types
(ql:quickload :defstar)    ;; add type annotations
(ql:quickload :str)        ;; Common string manipulation functions
(ql:quickload :parse-float)
(ql:quickload :iterate)
(ql:quickload :let-plus)          ;; extend "let"
(ql:quickload :array-operations)  ;; rich management of arrays

(defpackage :main
  (:import-from :alexandria)
  (:import-from :trivial-types :proper-list :tuple)
  (:use :cl :defstar :trivia :parse-float :iterate :let-plus)
  (:export main))

(in-package :main)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defun make-empty-group-arr ()
  (make-array 9 :element-type 'integer :initial-element 0))

(defun group-arr-init! (arr initial-group)
  (iter (for n in-sequence initial-group)
        (after-each (incf (aref arr n)))))

(defun day6a ()
  (let* ((in *standard-input*)
           (initial-group (map 'list #'parse-integer (str:split-omit-nulls "," (read-line in))))
           (days (parse-integer (read-line in)))
           (arr1 (make-empty-group-arr))
           (arr2 (make-empty-group-arr)))

    (group-arr-init! arr1 initial-group)

    (iter (for day from 1 to days)
          (after-each
            (let* ((g0 (aref arr1 0))
                   (g7 (aref arr1 7))
                   (g8 (aref arr1 8)))

               (iter (for i from 1 to 6)
                     (for j = (- i 1))
                     (after-each
                      (setf (aref arr2 j) (aref arr1 i))))

               (setf (aref arr2 8) g0)
               (setf (aref arr2 7) g8)
               (setf (aref arr2 6) (+ g0 g7))
               (rotatef arr1 arr2)))

          (finally
             (return
               (values
                  (iter (for n in-sequence arr1) (sum n))
                  arr1))))))

(defun main () (format t "~a~%" (day6a)))
