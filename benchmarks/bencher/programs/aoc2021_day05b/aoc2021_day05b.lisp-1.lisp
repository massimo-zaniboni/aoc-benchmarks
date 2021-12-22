
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

(defun parse-dgm-line (s)
  (let+ (((x1y1 x2y2) (str:split-omit-nulls " -> " s))
         ((x1 y1) (str:split-omit-nulls "," x1y1))
         ((x2 y2) (str:split-omit-nulls "," x2y2)))
    (values (parse-integer x1) (parse-integer y1) (parse-integer x2) (parse-integer y2))))

(parse-dgm-line "0,9 -> 5,9")
; => 0, 9, 5, 9

(defun dgm-inc (x1 x2)
  (cond
    ((= x1 x2) 0)
    ((< x1 x2) 1)
    ((> x1 x2) -1)))

(defun day5b ()
  (iter (for line in-stream *standard-input* using #'read-line)
        (with dgm = (make-hash-table :test #'equal))
        (for (values x1 y1 x2 y2) = (parse-dgm-line line))
        (for inc-x = (dgm-inc x1 x2))
        (for inc-y = (dgm-inc y1 y2))
        (after-each
          (iter (for x first x1 then (+ x inc-x))
                (for y first y1 then (+ y inc-y))
                (after-each
                   (incf (gethash `(,x ,y) dgm 0))
                   (when (and (= x x2) (= y y2)) (finish)))))
        (finally
          (return
            (iter (for (nil collisions) in-hashtable dgm)
                  (with c = 0)
                  (after-each (when (>= collisions 2) (incf c)))
                  (finally (return c)))))))

(defun main () (format t "~a~%" (day5b)))
