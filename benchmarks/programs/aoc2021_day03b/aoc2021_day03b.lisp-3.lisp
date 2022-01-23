
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
(ql:quickload :calispel)
(ql:quickload :jpl-queues)
(ql:quickload :eager-future2)

(defpackage :main
  (:import-from :alexandria)
  (:import-from :trivial-types :proper-list :tuple)
  (:use :cl :defstar :trivia :parse-float :iterate :let-plus :cffi)
  (:export main))

(in-package :main)

;; ## Design
;;
;; The slowest part in the original CL algo was reading the file and parsing numbers.
;; So a very mundane task.
;; The initialization of Judy array was fast enough and the calculation of results was immediate,
;; because there is few data to retrieve from the Judy array, i.e. the bulk of the work is done from
;; the Judy array C data structure.
;;
;; The code is redesigned in this way:
;; * a reading buffer is used for speeding up file read
;; * parsing of numbers is done on a distinct thread, with two benefits: IO can be scheduled and another core is used
;; * Judy array updates are node on a distinct thread
;; * buffers are allocated only one time and then reused
;; * buffers contain a big chunk of data, so work is done in batch mode, favouring reuse of cache and RAM locality


;; ## Low level settings

; TODO
; (proclaim '(optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
; TODO (setq *block-compile-default* t)

; Optimize only during compilation to an external file,
; but not during normal development.
; (eval-when (:compile-toplevel)
;  (proclaim '(optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0))))

  ; NOTE: inform that top level functions are not redefined at run-time,
  ; and they can be heavily optimized during compilation
  ; TODO (setq *block-compile-default* t))

;; ## Import Judy arrays data structure from C

; TODO call pkg-config and then use its result for loading the library
; TODO remove my machine name
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

;; ## Day 03b exsercise

;; ### Messages between process

(defconstant  +group-by+ 60
  "Assume 64 bit architecture, but use less bits than 64 bits for avoiding negative sign problems and accomodating for tag bits used by CL runtime")

(defconstant +most-significative-group-bit+ (- +group-by+ 1))

(defconstant +read-buffer-bytes+ 32768
  "Read data from disk in chunks")

(defconstant +judy-index-buffer-size+ 3000
  "How many indexes of 64 bit, put in the buffer")

; MAYBE switch to simple-array
(deftype read-byte-buffer () `(array (unsigned-byte 8) (,+read-buffer-bytes+)))

(defun make-read-byte-buffer ()
  (make-array +read-buffer-bytes+
              :element-type '(unsigned-byte 8)
              :adjustable nil))

(defstruct read-byte-buffer-msg
  (size 0 :type fixnum)
  (buff (make-read-byte-buffer)
        :type read-byte-buffer
        :read-only t))

(defun* (short-string -> string) ((s string))
  (format nil "~s" (subseq s 0 (min 40 (length s)))))

(defun* (read-byte-buffer-msg->show -> string) ((msg read-byte-buffer-msg))
  (format nil
          "Sent ~a chars"
          (read-byte-buffer-msg-size msg)))

; MAYBE switch to simple-array
(deftype judy-index () (upgraded-array-element-type `(signed-byte ,+group-by+)))

(deftype judy-index-buffer () `(array judy-index (,+judy-index-buffer-size+)))

(defun make-judy-index-buffer ()
  (make-array +judy-index-buffer-size+
              :element-type 'judy-index
              :adjustable nil))

(defstruct judy-index-buffer-msg
  (size 0 :type fixnum)
  (bits# 0 :type fixnum)
  (buff (make-judy-index-buffer)
        :type judy-index-buffer
        :read-only t))

(defun* (judy-index-buffer-msg->show -> string) ((msg judy-index-buffer-msg))
  (let* ((s (make-string-output-stream))
         (buff (judy-index-buffer-msg-buff msg))
         (c (judy-index-buffer-msg-size msg))
         (mc (min 40 c)))

  (format s "Sent ~a indexes:~%" c)
  (iter (for i from 0 to mc)
        (for v = (aref buff i))
        (after-each
           (format s "~2R  (~a)~%" v v)))

  (get-output-stream-string s)))

(defun read-input-file (s token-chan out-chan)
  (iter (for (the read-byte-buffer-msg msg) = (calispel:? token-chan))
        (for size = (read-sequence (read-byte-buffer-msg-buff msg) s))
        (after-each
           (setf (read-byte-buffer-msg-size msg) size)
           ; NOTE: send also the final 0 EOF message
           (calispel:! out-chan msg)
           (when (zerop size) (finish)))))

(defun read-byte-buffer-msg->bits# (msg)
  "Calculate how many bits are in each line"
  (iter (for i from 0 below (read-byte-buffer-msg-size msg))
        (for c = (aref (read-byte-buffer-msg-buff msg) i))
        (until (= c 10))
        (finally (return i))))

;; ### Process

(defun parse-input-file (index-token-chan in-chan out-chan token-chan)
  (iter (with (the judy-index-buffer-msg out-msg) = (calispel:? index-token-chan))
        (with (the judy-index-buffer out-buff) = (judy-index-buffer-msg-buff out-msg))
        (with vb = (make-sequence '(vector bit) +group-by+))
        (with vbi = 0) ; NOTE: 0 is the most significative bit
        (with out-size = 0)
        (with (the fixnum bits#) = 0)
        (for (the read-byte-buffer-msg msg) = (calispel:? in-chan))
        (for (the fixnum msg-size) = (read-byte-buffer-msg-size msg))
        (for (the read-byte-buffer buff) = (read-byte-buffer-msg-buff msg))
        (if-first-time (setf bits# (read-byte-buffer-msg->bits# msg)))
        (with send-out = (lambda (bits# last?)
                             (setf (judy-index-buffer-msg-bits# out-msg) bits#)
                             (setf (judy-index-buffer-msg-size out-msg) out-size)
                             (calispel:! out-chan out-msg)

                             ; wait for another free token to complete
                             (unless last?
                               (setf out-msg (calispel:? index-token-chan))
                               (setf out-size 0)
                               (setf out-buff (judy-index-buffer-msg-buff out-msg)))))

        (with send-vb = (lambda (bits#)
                           ; clean the rest of the bits
                           (iter (for j from vbi below +group-by+) (after-each (setf (aref vb j) 0)))
                           (setf vbi 0)

                           (setf (aref out-buff out-size)
                                 (coerce (bit-smasher:bits->int vb)
                                         'judy-index))

                            (incf out-size)
                            (when
                                (= out-size +judy-index-buffer-size+)
                                (funcall send-out bits# nil))))
        (until (zerop msg-size))
        (after-each
           ; Consume the content of the msg
           (iter (for i from 0 below msg-size)
                 (for c = (aref buff i))
                 (after-each
                    (cond
                        ((= c 10) (funcall send-vb bits#))
                        (t  (setf (aref vb vbi) (if (= c 48) 0 1))
                            (incf vbi)
                            (when (= vbi +group-by+) (funcall send-vb bits#))))))

           ; Reuse the consumed message, i.e. it is like a token
           (calispel:! token-chan msg))

        (finally
           (if (> vbi 0) (funcall send-vb bits#))
           (if (> out-size 0) (funcall send-out bits# nil))

           ; send final EOF msg
           (funcall send-out bits# t))))

(defun create-judy-tree (index-chan token-chan)
  "Store the numbers inside a compressed Judy array."

  (iter
     (with root-tree! = (foreign-alloc :pointer :initial-element (null-pointer)))
     (with slot! = root-tree!)
     (with pass = 0)
     (with (the fixnum bits#) = 0)
     (with (the fixnum pass#) = 0)
     (with (the fixnum last-shift) = 0)
     (for (the judy-index-buffer-msg msg) = (calispel:? index-chan))
     (for (the judy-index-buffer buff) = (judy-index-buffer-msg-buff msg))
     (for (the fixnum size) = (judy-index-buffer-msg-size msg))
     (if-first-time
       (setf bits# (judy-index-buffer-msg-bits# msg))
       (setf pass# (ceiling bits# +group-by+))
       (setf last-shift
          (if (= bits# +group-by+)
              0
              (- (- +group-by+ (mod bits# +group-by+))))))
     (until (zerop size))
     (after-each
       ; Add each index inside the Judy array.
       ; Decompose the long line in chunks of +group-by+ bit and store them in nested judy-trees.
       ; The last JudyTree of the sequence is a Judy1 set.
       ; NOTE:
       ; * each tree is a pointer pointing to the new root of the tree, after the insertion of the indeex
       ; * each slot is a placeholder where putting the value associated to the index, but its change does not afect the root of the tree
       (iter (for i from 0 below size)
             (for (the judy-index v) = (aref buff i))
             (after-each
                (let* ((last-pass? (= pass (- pass# 1)))
                       (adjusted-v (if last-pass? (ash v last-shift) v)))

                       (cond
                         (last-pass?
                           (judy-1-set! slot! adjusted-v)
                           (setf slot! root-tree!)
                           (setf pass 0))
                         (t (setf slot! (judy-l-ins slot! adjusted-v))
                            (incf pass)))))
             (finally (return nil)))

       ; Reuse the consumed message, i.e. it is like a token
       (calispel:! token-chan msg))
     (finally (return (values (mem-ref root-tree! :pointer) pass# bits#)))))

(defun file->judy-tree-profile-only-read (s)
  (iter
          (for line in-stream s using #'read-line)
          (for v = (parse-integer line :radix 2))
          (for r first v then (mod (+ r v) 275375325))
          (finally (return r))))

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
     (with (the fixnum last-pass-bits#) = 0)
     (for pass from 0 below pass#)
     (if-first-time
        (setf last-pass-bits# (mod bits# +group-by+)))
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

(defun scan-judy-1 (tree)
  (iter (with index! = (foreign-alloc :long :initial-element 0))
        (for continue? first (judy-1-first tree index!) then (judy-1-next tree index!))
        (while (= continue? 1))
        (collecting (format nil "~2R" (mem-ref index! :long)))))

(defun day3b2 (s)
  (let ((eager-future2:*default-future-type* :eager))
    (let* ((token-file-chan
             (make-instance 'calispel:channel
               :buffer (make-instance 'jpl-queues:bounded-fifo-queue :capacity 8)))

           (file-chan
             (make-instance 'calispel:channel
               :buffer (make-instance 'jpl-queues:bounded-fifo-queue :capacity 8)))

           (token-index-chan
             (make-instance 'calispel:channel
               :buffer (make-instance 'jpl-queues:bounded-fifo-queue :capacity 8)))

           (index-chan
             (make-instance 'calispel:channel
               :buffer (make-instance 'jpl-queues:bounded-fifo-queue :capacity 8)))

           (p1 (eager-future2:pexec (read-input-file s token-file-chan file-chan)))

           (p2 (eager-future2:pexec (parse-input-file token-index-chan file-chan index-chan token-file-chan)))

           (p3 (eager-future2:pexec (create-judy-tree index-chan token-index-chan))))

    ; Create resources that will be reused as token
    (dotimes (i 4 nil)
      (calispel:! token-file-chan (make-read-byte-buffer-msg))
      (calispel:! token-index-chan (make-judy-index-buffer-msg)))

    (eager-future2:yield p1)
    (eager-future2:yield p2)

    (multiple-value-bind
      (tree pass# bits#)
      (eager-future2:yield p3)
      (let* ((oxygen (judy-tree-rating tree pass# bits# t))
             (co2 (judy-tree-rating tree pass# bits# nil)))
         (values (* oxygen co2) oxygen co2))))))

(defun fn-day3b2 (fn)
   (with-open-file (s fn :direction :input :element-type '(unsigned-byte 8))
     (day3b2 s)))


; TODO delete
(defconstant +big-file+ #P"/home/zanibonim/tmp/advent-of-code/datasets-01/aoc2021_day3a/aoc2021-day3a-input100000.txt")
(defconstant +small-file+ #P"data/day-3-test.txt")
(defconstant +normal-file+ #P"data/day-3.txt")

; (fn-day3b2 +small-file+)
 ; => 230, 23, 10

; (fn-day3b2 +normal-file+)
 ; => 4105235, 2735, 1501

; (sb-profile:profile read-input-file read-byte-buffer-msg->bits parse-input-file create-judy-tree bit-smasher:bits->int)

(defun test () (fn-day3b2 +big-file+))
 ; => 356913958942791247617705918285570893096041618195840162127310, 461087838160523256683085674642, 774069340815133614804932891055

(defun test2 () (fn-day3b2 #P"../aoc2021_day03b-input100000.txt"))

(defun main () (format t "~a~%" (day3b2 *standard-input*)))

#|

## Before optimizations

(sb-profile:report)
measuring PROFILE overhead..done
  seconds  |     gc     |    consed   |   calls   |  sec/call  |  name
-------------------------------------------------------------
     2.481 |      0.000 |           0 | 2,000,050 |   0.000001 | PARSE-INTEGER
     1.485 |      0.140 | 463,367,952 | 1,000,002 |   0.000001 | READ-LINE
     1.441 |      0.000 |  18,972,768 |         1 |   1.440943 | MAIN::FILE->JUDY-TREE
     0.000 |      0.000 |           0 |         4 |   0.000115 | MAIN::JUDY-TREE-RATING-PASS
     0.000 |      0.000 |           0 |         2 |   0.000055 | MAIN::JUDY-TREE-RATING
-------------------------------------------------------------
     5.408 |      0.140 | 482,340,720 | 3,000,059 |            | Total

estimated total profiling overhead: 6.21 seconds
overhead estimation parameters:
  1.0708e-8s/call, 2.069246e-6s total profiling, 9.56748e-7s internal profiling
; No values

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
