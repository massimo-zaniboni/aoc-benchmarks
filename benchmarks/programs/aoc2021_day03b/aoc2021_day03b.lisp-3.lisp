
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
(ql:quickload :group-by)
(ql:quickload :cffi)
(ql:quickload :bit-smasher)
(ql:quickload :bordeaux-threads)
(ql:quickload :serapeum)
(ql:quickload :check-it)
(ql:quickload :parachute)

(defpackage :main
  (:import-from :alexandria)
  (:import-from :trivial-types :proper-list :tuple)
  (:use :cl :defstar :trivia :parse-float :iterate :let-plus :cffi)
  (:import-from :parachute :define-test :is :test :fail)
  (:import-from :group-by :group-by)
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
;; * there is a thread per core
;; * each thread read from the file, and update a local Judy array
;; * final queries on Judy arrays, for calculating the result are done in sequence,
;;   because they are few

;; ## Initial definitions

(declaim (type fixnum +read-buff-size+))
(defconstant +read-buff-size+ (ash 1 16))

(declaim (type fixnum +initial-bits+))
(defconstant +initial-bits+ 26)

(deftype ubyte8 () '(unsigned-byte 8))

;; ## Import Judy array data structure from C

(define-foreign-library libJudy
  (:unix "libJudy.so")
  (t (:default "libJudy")))

(use-foreign-library libJudy)

(defctype judy-tree! (:pointer :pointer))

(defctype judy-tree :pointer)

(defctype judy-value! (:pointer :ulong))

(defcfun "JudyLIns" :pointer (tree judy-tree!) (index :ulong) (error :pointer))

(defcfun "JudyLCount" :ulong (tree judy-tree) (index1 :ulong) (index2 :ulong) (error :pointer))

(defcfun "JudyLFirst" :pointer (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "JudyLLast" :pointer (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "JudyLPrev" :pointer (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "JudyLNext" :pointer (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "JudyLGet" :pointer (tree judy-tree) (index :ulong) (error :pointer))

(defcfun "Judy1Set" :int (tree judy-tree!) (index :ulong) (error :pointer))

(defcfun "Judy1First" :int (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "Judy1Last" :int (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "Judy1Count" :ulong (tree judy-tree) (index1 :ulong) (index2 :ulong) (error :pointer))

(defcfun "Judy1Prev" :int (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "Judy1Next" :int (tree judy-tree) (index! judy-value!) (error :pointer))

(defcfun "Judy1Test" :int (tree judy-tree) (index :ulong) (error :pointer))

(defmacro judy-l-ins-ptr (tree! index)
  `(foreign-funcall "JudyLIns" judy-tree! ,tree! :ulong ,index :pointer (null-pointer) (:pointer :pointer)))

(defmacro judy-l-ins-ulong (tree! index)
  `(foreign-funcall "JudyLIns" judy-tree! ,tree! :ulong ,index :pointer (null-pointer) (:pointer :ulong)))

(declaim (inline judy-l-ins-ulong!))
(defun judy-l-ins-ulong! (tree! index value)
  (let ((ptr (judy-l-ins-ulong tree! index)))
  (setf (mem-ref ptr :ulong) value)))

(declaim (inline judy-l-ins-ptr!))
(defun judy-l-ins-ptr! (tree! index value-ptr)
  (let ((ptr (judy-l-ins-ptr tree! index)))
  (setf (mem-ref ptr :pointer) value-ptr)))

(defmacro judy-l-first-ptr (tree index!)
  `(foreign-funcall "JudyLFirst" judy-tree ,tree judy-value! ,index! :pointer (null-pointer) :pointer))

(defmacro judy-l-next-ptr (tree index!)
  `(foreign-funcall "JudyLNext" judy-tree ,tree judy-value! ,index! :pointer (null-pointer) :pointer))

(defmacro judy-l-prev-ptr (tree index!)
  `(foreign-funcall "JudyLPrev" judy-tree ,tree judy-value! ,index! :pointer (null-pointer) :pointer))

(defmacro judy-l-last-ptr (tree index!)
  `(foreign-funcall "JudyLLast" judy-tree ,tree judy-value! ,index! :pointer (null-pointer) :pointer))

(defmacro judy-l-get-ptr (tree index)
  `(foreign-funcall "JudyLGet" judy-tree ,tree :ulong ,index :pointer (null-pointer) :pointer))

(defmacro judy-1-set! (tree! index)
  `(foreign-funcall "Judy1Set" judy-tree! ,tree! :ulong ,index :pointer (null-pointer) :int))

(defmacro judy-1-first (tree index!)
  `(foreign-funcall "Judy1First" judy-tree ,tree judy-value! ,index! :pointer (null-pointer) :int))

(defmacro judy-1-last (tree index!)
  `(foreign-funcall "Judy1Last" judy-tree ,tree judy-value! ,index! :pointer (null-pointer) :int))

(defmacro judy-1-next (tree index!)
  `(foreign-funcall "Judy1Next" judy-tree ,tree judy-value! ,index! :pointer (null-pointer) :int))

(defmacro judy-1-prev (tree index!)
  `(foreign-funcall "Judy1Prev" judy-tree ,tree judy-value! ,index! :pointer (null-pointer) :int))

(defmacro judy-1-test (tree index)
  `(foreign-funcall "Judy1Test" judy-tree ,tree :ulong ,index :pointer (null-pointer) :int))

(defmacro judy-1-count (tree index1 index2)
  `(foreign-funcall
               "Judy1Count"
               judy-tree ,tree
               :ulong ,index1
               :ulong ,index2
               :pointer (null-pointer)
               :ulong))

(defmacro judy-l-count (tree index1 index2)
  `(foreign-funcall
               "JudyLCount"
               judy-tree ,tree
               :ulong ,index1
               :ulong ,index2
               :pointer (null-pointer)
               :ulong))


(declaim (inline last-pass?))
(defun last-pass? (pass# pass)
  (= pass (- pass# 1)))

(defun judy-1-scan (s tree indent)
  "Show the content of a Judy 1 array."

  (iter (with index! = (foreign-alloc :ulong :initial-element 0))
        (for continue? first (judy-1-first tree index!) then (judy-1-next tree index!))
        (while (= continue? 1))
        (after-each
          (iter (for i from 0 to indent)
                (after-each (format s "..")))
          (format s "~2R~%" (mem-ref index! :ulong)))))

(defun judy-l-scan (s tree pass# pass)
  "Show the content of nested Judy arrays."

  (iter (with is-last-pass? = (last-pass? pass# pass))
        (with is-penultimate-pass? = (last-pass? pass# (+ pass 1)))
        (with index! = (foreign-alloc :ulong :initial-element 0))
        (for c from 0)
        (for ptr first (judy-l-first-ptr tree index!) then (judy-l-next-ptr tree index!))
        (until (null-pointer-p ptr))
        (after-each
          (iter (for i from 0 to pass)
                (after-each (format s "..")))
          (format s "element ~a: ~2R~%" c (mem-ref index! :ulong))
          (unless is-last-pass?
            (if is-penultimate-pass?
                (judy-1-scan s (mem-ref ptr :pointer) (+ pass 1))
                (judy-l-scan s (mem-ref ptr :pointer) pass# (+ pass 1)))))))

(defun judy-scan (s tree pass# pass)
  (if (last-pass? pass# pass)
      (judy-1-scan s tree pass)
      (judy-l-scan s tree pass# pass)))


;; ## Day 03b exsercise

(deftype read-buffer () '(simple-array ubyte8 1))

(deftype bitmask () '(simple-array bit 1))

(defconstant +max-bits+ (- (max (integer-length most-positive-fixnum) (integer-length most-negative-fixnum)) 1)
  "After these bits, native numbers are not any more nice inside CL run-time, because they will be big numbers.
I subtract 1 bit for avoiding problems with signed fixnum, also if probably not necessary.")

(defconstant +group-by+ +max-bits+)

(deftype judy-value ()
  "A judy-value as a CL friendly type."
  `(unsigned-byte ,+group-by+))

(defconstant judy-value-min (coerce 0 'judy-value))

(defconstant judy-value-max
  (iter (for i from 0 below +max-bits+)
        (for bm first (coerce 1 'judy-value) then (coerce (ash bm 1) 'judy-value))
        (for r first (coerce 1 'judy-value) then (coerce (boole boole-ior r bm) 'judy-value))
        (finally (return r))))

(declaim (type judy-value judy-value-min judy-value-max))

(deftype small-counter () '(signed-byte 16))

(defun* (reversed-first-line-bits->buffer -> read-buffer) ((bits# small-counter) (rev-bits (proper-list ubyte8)))
  (make-array
     bits#
     :adjustable nil
     :element-type 'ubyte8
     :initial-contents (reverse rev-bits)))

(defun* (parse-each-byte-of-the-first-line -> (values small-counter read-buffer)) ((s stream))
  "Parse slowly the first line, for detecting the number of bits."
  (iter (for v in-stream s :using #'read-byte)
        (until (or (null v) (= 10 v)))
        (counting v into c)
        (collecting v into vs at beginning)
        (finally (return (values c (reversed-first-line-bits->buffer c vs))))))

(declaim (inline insert-bits-into-judy!))
(defun* (insert-bits-into-judy! -> fixnum)
          ((root-tree! SB-SYS:SYSTEM-AREA-POINTER)
           (pass# small-counter)
           (last-pass-bits small-counter)
           (buffer read-buffer)
           (i fixnum))
  "Insert the bits in a chain of slots of Judy arrays:

   - bits are grouped in `+group-by+' bits, because a single index can be insufficient;
   - the last Judy is a Judy Set, having the bits in the index;
   - the penultimate Judy array put the first bits in the inedx, and the value is a pointer to the Judy set with other part of te bits;
   - and so on...

   Return the next position of the buffer to read.
   "

  (declare (optimize (speed 0) (debug 3) (safety 3)))

  (iter (with slot! = root-tree!)
        (for pass from 0 below pass#)
        (declare (dynamic-extent pass))
        (after-each
         (iter  (with index = (coerce 0 'judy-value))
                (declare (judy-value index))
                (with last-pass? = (= pass (- pass# 1)))
                (with bits-in-pass = (if last-pass? last-pass-bits +group-by+))
                (for j from (- bits-in-pass 1) downto 0)
                (declare ((or null small-counter) j))
                (declare (dynamic-extent index last-pass? bits-in-pass j))
                (after-each
                 (when (= 49 (the ubyte8 (aref buffer i)))
                   (setf index (boole boole-ior index (coerce (ash 1 j) 'judy-value))))
                  (incf i))
                (finally
                   ; skip newline
                   (when last-pass?
                     (incf i))

                   ; NOTE: Judy has a very compact API, but it is rather obscure.
                   (cond
                      (last-pass?
                        (judy-1-set! slot! index)
                        (setf slot! (mem-ref root-tree! :pointer)))
                      (t (setf slot! (judy-l-ins-ptr slot! index)))))))
        (finally (return i))))

(defun* (day3b-thread -> SB-SYS:SYSTEM-AREA-POINTER)
    ((s stream)
     (lock-s sb-thread:mutex)
     (pass# small-counter)
     (last-pass-bits small-counter)
     (buffer-size fixnum))
  "Use bytes because managing *input-stream* as characters (but not normal stream files), make it too much slower (10x more slower!).
   Manage the input stream s as a shared resource.

   Add each index inside the Judy array.
   Decompose the long line in chunks of +group-by+ bit and store them in nested judy-trees.
   The last JudyTree of the sequence is a Judy1 set.
"

  (iter
     (with root-tree! = (foreign-alloc :pointer :initial-element (null-pointer)))
     (with buffer = (make-array buffer-size :adjustable nil :element-type 'ubyte8))
     (declare (read-buffer buffer))

     (for end = (bt:with-lock-held (lock-s) (read-sequence buffer s)))
     (declare (fixnum end))

     (until (zerop end))

     (after-each
       ; Process each line of the buffer.
       ; NOTE: requires that `buffer-size' is aligned with `cols#'

       (iter (with i = (coerce 0 'fixnum))
             (declare (fixnum i))
             (declare (dynamic-extent i))
             (while (< i end))
             (after-each
               (setf i (insert-bits-into-judy! root-tree! pass# last-pass-bits buffer i)))))

     (finally (return (mem-ref root-tree! :pointer)))))

(defun* (judy-tree-rating-pass -> judy-value)
          ((trees (proper-list SB-SYS:SYSTEM-AREA-POINTER))
           (bits-in-pass small-counter)
           (last-pass? boolean)
           (follow-max? boolean))

  "Count for every bit the elements inside the Judy arrays with 0 and 1 values,
   until a unique element is found.
   Work on a list of distinct judy trees, produced by distinct threads.
   Do not consider Judy arrays nested, so it returns only the index of current pass."

   (iter
     (with result = (coerce 0 'judy-value))
     ; the current selected result, completede from most significative bits to lower

     (with index1 = (coerce 0 'judy-value))
     ; the minimum element that can be selected (comprehensive)

     (with index2 = judy-value-max)
     ; the maximum element that can be selected (comprehensive)

     (with tot-n = 0)
     ; the total number of elements to select in each tree

     (initially
       (iter (for tree in-sequence trees)
             (after-each
               (let* ((n (if last-pass?
                          (judy-1-count tree index1 index2)
                          (judy-l-count tree index1 index2))))
                 (incf tot-n n)))))

     (for i from (- bits-in-pass 1) downto 0)
     (for bitmask-on-i = (coerce (ash 1 i) 'judy-value))
     ; Select each bit counting the elements with this bit set to 0 or 1,
     ; starting from most significative bit

     (after-each
      (assert (> tot-n 0))

        (setf result (boole boole-ior result bitmask-on-i))
        ; consider the next higher possible value in the bit

        (let* ((tot-0b 0)
               ; how many elements there are with this bit set to 0

               (tot-1b 0))

          ; Collect totals for each tree
          (iter (for tree in-sequence trees)
                (after-each
                  (incf tot-1b (if last-pass?
                                   (judy-1-count tree result index2)
                                   (judy-l-count tree result index2)))))

          (setf tot-0b (- tot-n tot-1b))

         ; Decide the bit to follow, according the total sums of 0 and 1 bits
          (let* ((empty-1bit? (zerop tot-1b))
                 (empty-0bit? (zerop tot-0b))
                 (take-1bit?  (cond
                                (empty-1bit? nil)
                                (empty-0bit? t)
                                (t (if follow-max?
                                       (>= tot-1b tot-0b)
                                       (< tot-1b tot-0b))))))

          (assert (not (and empty-1bit? empty-0bit?)))
          (cond
             (take-1bit?
              (setf tot-n tot-1b)
              (setf index1 result))
             (t
               ; take 0 bit, instead

               (setf tot-n tot-0b)

               (setf index2 (- result 1))
               ; the new bottom limit is a number with 1 in the rest of the bits.
               ; This is required because bottom limit is comprehensive, and not exclusive

               ; revert to 0 the bit, instead to 1
               (setf result (boole boole-xor result bitmask-on-i)))))))
     (finally (return result))))

(defun* (judy-tree-rating -> integer)
          ((trees (proper-list SB-SYS:SYSTEM-AREA-POINTER))
           (pass# small-counter)
           (bits# small-counter)
           (last-pass-bits small-counter)
           (follow-max? boolean))

  "Apply judy-tree-rating-pass to each nested judy-tree, i.e. each nested judy-tree is another pass
containing lesser significative bits, until last-pass is reached.

The returned integer can be bigger than judy-value, because it can be a multi-word value."

  (iter
     (with curr-trees = trees)
     (with result = 0)
     (with left-bits = bits#)
     (for pass from 0 below pass#)
     (for last-pass? = (= pass (- pass# 1)))
     (for bits-in-pass = (if last-pass? last-pass-bits +group-by+))
     (after-each
        (let* ((index (judy-tree-rating-pass curr-trees bits-in-pass last-pass? follow-max?)))
          (unless last-pass?
                  (setf curr-trees
                        (iter
                          (for curr-tree in-sequence curr-trees)
                          (for ptr = (judy-l-get-ptr curr-tree index))
                          (when (null-pointer-p ptr) (next-iteration))
                          (collecting (mem-ref ptr :pointer) at beginning))))

          (decf left-bits bits-in-pass)
          (incf result (ash index left-bits))))
     (finally (return result))))

(defun day3b-main-thread (s)
  (bt:start-multiprocessing)

  (multiple-value-bind (bits# first-line-bits)
    (parse-each-byte-of-the-first-line s)
    (declare (small-counter bits#))
    (let* ((cols# (+ bits# 1))
           (last-pass-bits (if (= bits# +group-by+) +group-by+ (mod bits# +group-by+)))
           (buffer-lines# (coerce (ceiling +read-buff-size+ cols#) 'fixnum))
           (buffer-size (* cols# buffer-lines#))
           (pass# (ceiling bits# +group-by+))

           (cores# (serapeum:count-cpus))

           (lock-s (bt:make-lock))
           (threads
             (iter (for i from 0 below cores#)
                   (collect
                       (bt:make-thread
                        (lambda ()
                          (day3b-thread s lock-s pass# last-pass-bits buffer-size)))
                    at beginning)))

           (judy-trees
             (remove-if #'null-pointer-p (map 'list #'bt:join-thread threads))))

          (declare (small-counter last-pass-bits))

          ; Add the first line to a judy tree
          (let ((car-tree! (foreign-alloc :pointer :initial-element (car judy-trees))))
            (insert-bits-into-judy! car-tree! pass# last-pass-bits first-line-bits 0)
            (setf judy-trees (cons (mem-ref car-tree! :pointer) (cdr judy-trees))))

          (let* ((oxygen (judy-tree-rating judy-trees pass# bits# last-pass-bits t))
                 (co2 (judy-tree-rating judy-trees pass# bits# last-pass-bits nil)))
             (values (* oxygen co2) oxygen co2)))))


(defun day3b-main-thread-fn (fn)
  (with-open-file (s fn :element-type 'ubyte8)
    (day3b-main-thread s)))

(defun main () (format t "~a~%" (day3b-main-thread *standard-input*)))
