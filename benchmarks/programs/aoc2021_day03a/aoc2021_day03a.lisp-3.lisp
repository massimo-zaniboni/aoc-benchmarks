
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
(ql:quickload :bordeaux-threads)
(ql:quickload :serapeum)

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


(defun day3a-byte-thread (s lock-s bits# cols# buffer-size)
  "Use bytes because managing *input-stream* as characters (but not normal stream files), make it too much slower (10x more slower!).
   Manage the input stream s as a shared resource."

  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (fixnum bits# cols# buffer-size))

    (iter
      (with count-bits = (make-array bits# :adjustable nil :element-type 'fixnum :initial-element 0))
      (declare (type (simple-array fixnum 1) count-bits))
      (with buffer = (make-array buffer-size :adjustable nil :element-type 'ubyte8))
      (declare (type (simple-array ubyte8 1) buffer))
      (with count-lines = 0)
      (for end = (the fixnum (bt:with-lock-held (lock-s) (read-sequence buffer s))))
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
      (finally (return (values count-lines count-bits)))))

(defun* (parse-each-byte-of-the-first-line -> (values fixnum (simple-array fixnum 1))) ((s stream))

  (multiple-value-bind (bits# reverse-bits)
    (iter
      (for v in-stream s :using #'read-byte)
      (until (or (null v) (= 10 v)))
      (counting v into c)
      (collect (coerce (- v 48) 'fixnum) into bits at beginning)
      (finally (return (values c bits))))

  (declare (fixnum bits#))
  (declare (type (proper-list fixnum) reverse-bits))

  (values bits#
          (make-array bits#
                      :adjustable nil
                      :element-type 'fixnum
                      :initial-contents (nreverse reverse-bits)))))

(defun day3a-byte-main-thread (s)
  (bt:start-multiprocessing)

  (multiple-value-bind (bits# count-bits)
    (parse-each-byte-of-the-first-line s)
    (declare (fixnum bits#))
    (declare (type (simple-array fixnum 1) count-bits))
    (let* ((cols# (the fixnum (+ bits# 1)))

           (buffer-lines# (coerce (ceiling +read-buff-size+ cols#) 'fixnum))

           (buffer-size (the fixnum (* cols# buffer-lines#)))

           (cores# (serapeum:count-cpus))

           (lock-s (bt:make-lock))

           (threads
             (iter (for i from 0 below cores#)
                   (collect
                       (bt:make-thread (lambda () (day3a-byte-thread s lock-s bits# cols# buffer-size)))
                    at beginning)))

           (count-lines (coerce 1 'fixnum))
           )

         ; NOTE: I'm not using iter macro anymore because it set vars to nil,
         ; creating type problems during compilation
         (dolist (job threads)
           (multiple-value-bind
              (count-lines1 count-bits1) (bt:join-thread job)
              (declare (fixnum count-lines1))
              (declare (type (simple-array fixnum 1) count-bits1))

              (incf count-lines count-lines1)
              (dotimes (i bits#)
                (incf (aref count-bits i)
                      (aref count-bits1 i)))))

         (count-bits->gamma-epsilon count-lines bits# count-bits))))

(defun day3a-byte-main-thread-fn (fn)
  (with-open-file (s fn :element-type 'ubyte8)
    (day3a-byte-main-thread s)))

(defun main ()
  (format t "~a~%" (day3a-byte-main-thread *standard-input*)))
