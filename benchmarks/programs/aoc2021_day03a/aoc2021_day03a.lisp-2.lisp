
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; Copyright (C) 2021 Massimo Zaniboni <mzan@dokmelody.org>

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

(declaim (type fixnum +read-buff-size+))
(defconstant +read-buff-size+ (ash 1 16))

(declaim (type fixnum +initial-bits+))
(defconstant +initial-bits+ 26)

(deftype ubyte8 () '(unsigned-byte 8))

(defun count-bits->gamma-epsilon (max-count bits# count-bits)

  (iter (for i from (- bits# 1) downto 0)
        (for cb = (aref count-bits i))
        (for e first 1 then (* e 2))
        (with mc = (/ max-count 2))
        (with gamma = 0)
        (with epsilon = 0)
        (after-each
           (cond
             ((> cb mc) (setf gamma (+ gamma e)))
             (t (setf epsilon (+ epsilon e)))))
       (finally
        (return (values (* gamma epsilon) gamma epsilon max-count bits#)))))

(defun* (parse-first-line -> (values fixnum (simple-array ubyte8 1) fixnum)) ((s stream))
  "Calculate how many bits are in each line."
  (iter (with max-bits = (* 10 64)) ; NOTE: assume that there are not more than these bits
        (with buffer = (make-array max-bits :adjustable nil :element-type 'ubyte8 :initial-element 0))
        (with end = (read-sequence buffer s))
        (for i from 0 below end)
        (declare (fixnum i))
        (for c = (aref buffer i))
        (until (= c 10))
        (finally
         (assert (= c 10))
         (return (values i buffer end)))))

(defun day3a-byte (s)
  "Use bytes because managing *input-stream* as characters (but not normal stream files), make it too much slower (10x more slower!)"

  (declare (optimize (speed 3) (debug 0) (safety 0)))

  (let+ (((&values bits# initial-buffer initial-end) (parse-first-line s))
         (cols# (the fixnum (+ 1 bits#)))
         (buffer-lines# (coerce (ceiling +read-buff-size+ cols#) 'fixnum))
         (buffer-size (the fixnum (* cols# buffer-lines#)))
         (buffer (make-array buffer-size :adjustable nil :element-type 'ubyte8))
         (first-end (progn
                      (assert (>= buffer-size initial-end))
                      (if (> buffer-size initial-end)
                          (read-sequence buffer s :start initial-end)
                          initial-end)))
         (count-bits (make-array bits# :adjustable nil :element-type 'fixnum :initial-element 0)))

        (declare (type (simple-array ubyte8 1) initial-buffer buffer))
        (declare (type (simple-array fixnum 1) count-bits))
        (declare (fixnum bits# cols# buffer-lines# buffer-size first-end initial-end))

    ; Reuse the already loaded buffer, because *input-stream* cannot be rewinded
    (iter (for i from 0 below initial-end)
          (declare (fixnum i))
          (after-each (setf (aref buffer i) (aref initial-buffer i))))

    ; Process data
    (iter
      (with count-lines = 0)
      (for end first first-end then (read-sequence buffer s))
      (declare (fixnum count-lines end))
      (until (zerop end))
      (after-each
       (iter (with j = 0)
             (for i from 0 below end)
             (declare (fixnum i j))
             (declare (dynamic-extent i j))
             (after-each
               (case (the ubyte8 (aref buffer i))
                 (49 (incf (aref count-bits j)))
                 (10 (setf j -1)))
               (incf j))
             (finally (incf count-lines (ceiling end cols#)))))
      (finally (return (count-bits->gamma-epsilon count-lines bits# count-bits))))))

(defun day3a-byte-fn (fn)
  (with-open-file (s fn :element-type 'ubyte8)
    (day3a-byte s)))

(defun main ()
  (format t "~a~%" (day3a-byte *standard-input*)))
