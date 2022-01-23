
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
(ql:quickload :cffi)
(ql:quickload :bit-smasher)

(defpackage :main
  (:import-from :alexandria)
  (:import-from :trivial-types :proper-list :tuple)
  (:use :cl :defstar :trivia :parse-float :iterate :let-plus :cffi)
  (:export main))

(in-package :main)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(declaim (type fixnum +read-buff-size+))
(defconstant +read-buff-size+ (ash 1 16))

(declaim (type fixnum +initial-bits+))
(defconstant +initial-bits+ 26)

(deftype ubyte8 () '(unsigned-byte 8))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Import Judy arrays data structure from C to CL

(define-foreign-library libJudy
  (:unix "libJudy.so")
  (t (:default "libJudy")))

(use-foreign-library libJudy)

(defctype judy-tree! (:pointer :pointer))

(defctype judy-tree :pointer)

(defctype judy-value! (:pointer :long))

(defcfun "JudyLIns" judy-value! (tree judy-tree!) (index :long) (error :pointer))

(defcfun "JudyLCount" :unsigned-int (tree judy-tree) (index1 :long) (index2 :long) (error :pointer))

(defcfun "JudyLFirst" :pointer (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "JudyLLast" :pointer (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "JudyLPrev" :pointer (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "JudyLNext" :pointer (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "Judy1Set" :int (tree judy-tree!) (index :long) (error :pointer))

(defcfun "Judy1First" :int (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "Judy1Last" :int (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "Judy1Count" :unsigned-int (tree judy-tree) (index1 :long) (index2 :long) (error :pointer))

(defcfun "Judy1Prev" :int (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "Judy1Next" :int (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "Judy1Test" :int (tree judy-tree) (index :long) (error :pointer))

(defmacro judy-l-ins (tree! index)
  `(foreign-funcall "JudyLIns" judy-tree! ,tree! :long ,index :pointer ,(null-pointer) :pointer))

(defmacro judy-l-ins! (tree! index value)
  `(setf (mem-ref ,(judy-l-ins tree! index) :long) ,value))

(defmacro judy-l-first (tree index!)
  `(foreign-funcall "JudyLFirst" judy-tree ,tree judy-value! ,index! :pointer ,(null-pointer) :pointer))

(defmacro judy-l-next (tree index!)
  `(foreign-funcall "JudyLNext" judy-tree ,tree judy-value! ,index! :pointer ,(null-pointer) :pointer))

(defmacro judy-l-prev (tree index!)
  `(foreign-funcall "JudyLPrev" judy-tree ,tree judy-value! ,index! :pointer ,(null-pointer) :pointer))

(defmacro judy-l-last (tree index!)
  `(foreign-funcall "JudyLLast" judy-tree ,tree judy-value! ,index! :pointer ,(null-pointer) :pointer))

(defmacro judy-1-set! (tree! index)
  `(foreign-funcall "Judy1Set" judy-tree! ,tree! :long ,index :pointer ,(null-pointer) :int))

(defmacro judy-1-first (tree index!)
  `(foreign-funcall "Judy1First" judy-tree ,tree judy-value! ,index! :pointer ,(null-pointer) :int))

(defmacro judy-1-last (tree index!)
  `(foreign-funcall "Judy1Last" judy-tree ,tree judy-value! ,index! :pointer ,(null-pointer) :int))

(defmacro judy-1-next (tree index!)
  `(foreign-funcall "Judy1Next" judy-tree ,tree judy-value! ,index! :pointer ,(null-pointer) :int))

(defmacro judy-1-prev (tree index!)
  `(foreign-funcall "Judy1Prev" judy-tree ,tree judy-value! ,index! :pointer ,(null-pointer) :int))

(defmacro judy-1-test (tree index)
  `(foreign-funcall "Judy1Test" judy-tree ,tree :long ,index :pointer ,(null-pointer) :int))

(defmacro judy-1-count (tree index1 index2)
  `(foreign-funcall
               "Judy1Count"
               judy-tree ,tree
               :long ,index1
               :long ,index2
               :pointer ,(null-pointer)
               :unsigned-int))

(defmacro judy-l-count (tree index1 index2)
  `(foreign-funcall
               "JudyLCount"
               judy-tree ,tree
               :long ,index1
               :long ,index2
               :pointer ,(null-pointer)
               :unsigned-int))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Day 03b

; NOTE: use less bit than 64 bit for avoiding negative sign problems
(defparameter +group-by+ 62)

(defun file->judy-tree (s)
  "Store the numbers as ordered index of a (compressed) Judy array."
  (iter   (with root-tree! = (foreign-alloc :pointer :initial-element (null-pointer)))
          (for line in-stream s using #'read-line)
          (for bits# first (length line) then bits#)
          (for pass# first (ceiling bits# +group-by+) then pass#)
          (after-each
             ; Decompose the long line in chunks of +group-by+ bit and store them in nested judy-trees.
             ; The last JudyTree of the sequence is a Judy1 set.
             ; NOTE:
             ; * each tree is a pointer pointing to the new root of the tree, after the insertion of the indeex
             ; * each slot is a placeholder where putting the value associated to the index, but its change does not afect the root of the tree
             (iter
               (with slot! = root-tree!)
               (for line-start from 0 below bits# by +group-by+)
               (for pass from 0 below pass#)
               (for last-pass? = (= pass (- pass# 1)))
               (for pass-bits# = (if last-pass? (mod bits# +group-by+) +group-by+))
               (for line-end = (+ line-start pass-bits#))
               (for v = (parse-integer line :start line-start :end line-end :radix 2))
               (after-each
                 (if last-pass?
                     (judy-1-set! slot! v)
                     (setf slot! (judy-l-ins slot! v)))))
               (finally (return nil)))

          (finally (return (values (mem-ref root-tree! :pointer) pass# bits#)))))

(defun judy-tree-rating-pass (tree bits# last-pass? follow-max?)
  "Count for every bit the upper and lower elements inside the Judy array, until a unique element is found."
  (iter
     (with result = (make-sequence '(vector bit) bits# :initial-element 0))
     (with index1 = (foreign-alloc :long :initial-element 0))
     (with index2 = (foreign-alloc :long :initial-element -1))
     (with index! =  (foreign-alloc :long :initial-element 0))
     (with n = (if last-pass?
                   (judy-1-count tree (mem-ref index1 :long) (mem-ref index2 :long))
                   (judy-l-count tree (mem-ref index1 :long) (mem-ref index2 :long))))

     (for i from 0 below bits#)
      (after-each
        ; search for next bit
        (setf (sbit result i) 1)
        (setf (mem-ref index! :long) (bit-smasher:bits->int result))

        (if last-pass?
            (judy-1-first tree index!)
            (judy-l-first tree index!))

        (let* ((n1 (if last-pass?
                       (judy-1-count tree (mem-ref index! :long) (mem-ref index2 :long))
                       (judy-l-count tree (mem-ref index! :long) (mem-ref index2 :long))))
               (n0 (- n n1))
               (empty-1bit? (zerop n1))
               (empty-0bit? (zerop n0))
               (take-1bit?  (if follow-max?
                                (and (not empty-1bit?) (>= n1 n0))
                                (and (not empty-1bit?) (< n1 n0)))))

           (cond
             ((and empty-1bit? empty-0bit?)
                (leave (mem-ref index! :long)))

             (take-1bit?
                (setf (mem-ref index1 :long) (mem-ref index! :long))
                (setf n n1))
             (t
                ; ASSERT if it is greather than n1, then n0 > 0
                (setf (sbit result i) 0)
                (if last-pass?
                    (judy-1-prev tree index!)
                    (judy-l-prev tree index!))
                (setf (mem-ref index2 :long) (mem-ref index! :long))
                (setf n n0)))))
      (finally (return (bit-smasher:bits->int result)))))

(defun judy-tree-rating (tree pass# bits# follow-max?)
  "Apply judy-tree-rating-pass to each nested index of the array. Needed when indexes are wider than +group-by+"
  (iter
     (with curr-index = (foreign-alloc :long :initial-element 0))
     (with curr-tree = tree)
     (with (the bignum result) = 0)
     (for pass from 0 below pass#)
     (for last-pass-bits# first (mod bits# +group-by+) then last-pass-bits#)
     (for last-pass? = (= pass (- pass# 1)))
     (for pass-bits# = (if last-pass? last-pass-bits# +group-by+))
     (for shift-result = (cond
                           (last-pass? 0)
                           ((= pass 0) last-pass-bits#)
                           (t +group-by+)))
     (after-each
        (let* ((index (judy-tree-rating-pass curr-tree pass-bits# last-pass? follow-max?)))
          (unless last-pass?
                  (setf (mem-ref curr-index :long) index)
                  (setf curr-tree (mem-ref (judy-l-first curr-tree curr-index) :pointer)))

          (setf result (+ result (ash index shift-result)))))
     (finally (return result))))

(defun day3b2 (s)
    (let+ (((&values tree pass# bits#) (file->judy-tree s))
           (oxygen (judy-tree-rating tree pass# bits# t))
           (co2 (judy-tree-rating tree pass# bits# nil)))
      (values (* oxygen co2) oxygen co2)))

(defun main () (format t "~a~%" (day3b2 *standard-input*)))

#|
(day3b2 #P"data/day-3-test.txt")
 ; => 230, 23, 10

(day3b2 #P"data/day-3.txt")
 ; => 4105235, 2735, 1501

(day3b2 #P"/home/zanibonim/tmp/advent-of-code/datasets-01/aoc2021_day3b/aoc2021-day3b-input1000.txt")
 ; => 4105235, 2735, 1501

(day3b2 #P"/home/zanibonim/tmp/advent-of-code/datasets-01/aoc2021_day3a/aoc2021-day3a-input100000.txt")
 ; => 356913958942791247617705918285570893096041618195840162127310, 461087838160523256683085674642, 774069340815133614804932891055

;    1000001 lines each with a 100 bit number
;    Download 3-1000001-100.in.xz (14MiB compressed / 97MiB decompressed)
;    Part 1: 386463119445733053722557199393548794069517420395751036911156
;    Part 2: 356913958942791247617705918285570893096041618195840162127310
|#
