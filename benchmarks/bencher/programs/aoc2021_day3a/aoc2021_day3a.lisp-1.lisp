
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

(defun grow-vector-upto (vect new-size value)
  (iter (for i from (fill-pointer vect) below new-size)
        (vector-push-extend value vect)))

; # Day 3a

(defun count-bits->gamma-epsilon (max-count count-bits)
  (iter (for cb in-vector count-bits downto 0)
        (for e first 1 then (* e 2))
        (with mc = (/ max-count 2))
        (with gamma = 0)
        (with epsilon = 0)
        (after-each
           (cond
             ((> cb mc) (setf gamma (+ gamma e)))
             (t (setf epsilon (+ epsilon e)))))
       (finally (return (values (* gamma epsilon) gamma epsilon)))))

(defun day3a ()
    (iter (for line in-stream *standard-input* using #'read-line)
          (count line into count-lines)
          (with count-bits = (make-array 15 :fill-pointer 0 :adjustable t :element-type 'fixnum))
          (if-first-time (grow-vector-upto count-bits (length line) 0))
          (after-each
            (iter (for c in-string line)
                  (for i from 0 )
                  (when (char= c #\1) (incf (elt count-bits i)))))
          (finally
           (return (count-bits->gamma-epsilon count-lines count-bits)))))

(defun main () (format t "~a~%" (day3a)))
