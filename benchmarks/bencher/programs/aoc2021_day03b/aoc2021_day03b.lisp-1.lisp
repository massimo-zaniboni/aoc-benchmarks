
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

; # Day 3b

; I store the strings in a trie, so I can choose in an efficient way the more frequent character.

(defclass trie ()
  (
   (parent
    :type (or trie null)
    :documentation "The parent trie."
    :initform nil
    :accessor trie-parent
    :initarg :parent)
   (child-0
    :type (or trie null)
    :documentation "The children with prefix 0."
    :accessor trie-child-0
    :initform nil)
   (child-1
    :type (or trie null)
    :documentation "The children with prefix 1."
    :accessor trie-child-1
    :initform nil)
   (count-children
    :type integer
    :documentation "Count children nodes."
    :initform 0
    :accessor trie-count-children)
   )
  (:documentation "A trie of 0|1, counting the number of strings.")
  )

(defun* (trie-child -> (or trie null)) ((self trie) (prefix fixnum))
  "Select the child using a number as prefix."
  (if (zerop prefix)
      (trie-child-0 self)
      (trie-child-1 self)))

(defun* (trie-child-count -> integer) ((self trie) (prefix fixnum))
  "Count the children of each brach."
  (let ((child (trie-child self prefix)))
    (if (null child) 0 (trie-count-children child))))

(defun* (trie-add-prefix! -> trie) ((self trie) (prefix number))
  (let* ((child
           (if (zerop prefix)
               (trie-child-0 self)
               (trie-child-1 self)))
         (new-child
            (if (null child)
                (make-instance 'trie :parent self)
                child)))
    (if (zerop prefix)
        (setf (trie-child-0 self) new-child)
        (setf (trie-child-1 self) new-child))

    new-child))

(defun* (trie-show -> null) ((out stream) (self trie) (prefix string))
  (format out "(~s:~d " prefix (trie-count-children self))

  (unless (null (trie-child-0 self))
    (trie-show out (trie-child-0 self) (concatenate 'string prefix "0")))

   (unless (null (trie-child-1 self))
    (trie-show out (trie-child-1 self) (concatenate 'string prefix "1")))

  (format out " )"))

(defun* (char01->number -> number) ((c character))
  (cond
    ((char= c #\0) 0)
    ((char= c #\1) 1)
    (t (error "Unexpected character"))))

(defun* (trie-insert-string! -> null) ((self trie) (cs string))
  "Insert a string in the trie."
  (iter (for c in-string cs)
        (for tt initially self then (trie-add-prefix! tt (char01->number c)))
        (finally
          (iter (for ttt first tt then (trie-parent ttt))
            (while ttt)
            (after-each (incf (trie-count-children ttt))))
         (return nil))))

(defun* (file->trie -> trie) ()
  "Read numbers from file and produce the trie."
    (iter (for line in-stream *standard-input* using #'read-line)
          (with tt = (make-instance 'trie))
          (after-each (trie-insert-string! tt line))
          (finally (return tt))))

(defun* (seq01-rev->number -> number) ((self (proper-list (or number null))))
  "Convert a sequence of 0 and 1 in reverse order, to a number."
  (iter (for c in-sequence self)
        (for e first 1 then (* e 2))
        (if (not (null c)) (sum (* e c)))))

(defun* (trie-rating -> number) ((self trie) (follow-max? boolean))
  "Return oxygen or CO2 scrubber generator, following max or min digits."

  (iter (for tt01
             initially (cons self nil)
             then (let* ((tt (car tt01))
                         (child0 (trie-child-0 tt))
                         (child1 (trie-child-1 tt))
                         (c0 (trie-child-count tt 0))
                         (c1 (trie-child-count tt 1)))
                   (cond
                     ((and (zerop c0) (zerop c1)) (cons nil nil))
                     ((zerop c0) (cons child1 1))
                     ((zerop c1) (cons child0 0))
                     (t (if follow-max?
                            (if (>= c1 c0) (cons child1 1) (cons child0 0))
                            (if (<= c0 c1) (cons child0 0) (cons child1 1)))))))
        (while (car tt01))
        (collect (cdr tt01) at beginning into seq01-rev)
        (finally
           (return (seq01-rev->number seq01-rev)))))

(defun day3b ()
  (let* ((trie (file->trie))
         (oxygen (trie-rating trie t))
         (co2 (trie-rating trie nil)))
     (values (* oxygen co2) oxygen co2 (format nil "~B" oxygen) (format nil "~B" co2))))

(defun main () (format t "~a~%" (day3b)))
