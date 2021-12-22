
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


; # Utilities

(defun nil-min2 (x y)
  (if (null x)
      y
      (if (null y)
          x
          (min x y))))

(defun nil<= (x y)
  (if (null x)
      y
      (if (null y)
          x
          (<= x y))))

(defun nil>= (x y)
  (if (null x)
      y
      (if (null y)
          x
          (>= x y))))

; # Day 4b

; ## Solution design
;
; The idea is to replace numbers with their order of extraction.
; So a row with numbers "3 10 8"
; is replaced with extration order "8 1 2",
; hence it will be completed at 8th extraction.
; 8 is the winner-extraction.
;
; This approach simplifies comparison between rows and boards,
; because it suffices to store the maximum and minimum winniner-extraction.

; The extraction order of numbers.
(deftype extraction () `fixnum)

(defclass board ()
  (
   (number->extraction
    :type hash-table
    :documentation "Map a number to its extraction order."
    :accessor board-number->extraction
    :initarg :number->exctraction)
   (extraction->number
    :type (vector extraction)
    :documentation "From an extraction order like 3, to the corresponding number."
    :accessor board-extraction->number
    :initarg :extraction->number)
   (content
    :type (vector extraction)
    :documentation "The numbers inside the board, saved in extraction order format."
    :accessor board-content
    :initform (make-array 25 :element-type 'fixnum :fill-pointer 0 :adjustable t))
   (winner-extraction
    :type (or extraction null)
    :documentation "The board winning extraction, i.e. the first extraction completing a row."
    :initform nil
    :accessor board-winner-extraction)
   )
  (:documentation "A Bingo board where numbers are expressed according their extraction order.")
  )

(defun* (board-show -> string) ((self board))
  (with-output-to-string (out)
    (iter (for e in-sequence (board-content self))
          (for n = (aref (board-extraction->number self) e))
          (with we = (board-winner-extraction self))
          (for extracted? = (or (null we) (<= e we)))
          (for winner? = (and (not (null we)) (= e we)))
          (for box = (if winner?
                         #\*
                         (if extracted?
                             #\+
                             #\ )))
          (after-each (format out " ~a~a " n box)))))

(defun* (parse-board-extractions -> (tuple hash-table (vector extraction))) ((in stream))
  "Parse a line with the extractions."
  (let* ((nn (mapcar #'parse-integer (str:split "," (read-line in))))
         (extractions (make-array
                         (length nn)
                         :element-type 'fixnum
                         :initial-contents nn))

         (numbers (iter (for i index-of-vector extractions)
                        (with hash = (make-hash-table :size (length extractions)))
                        (after-each (setf (gethash (aref extractions i) hash) i))
                        (finally (return hash)))))

    (read-line in nil nil) ; skip an empty line
    (list numbers extractions)))

(defun* (board-add-row! -> null) ((self board) (row sequence) &key ((add-to-content? boolean) t))
  "Add a row (or column) to the board and maintain board-state."
  (iter (for n in-sequence row)
        (for e = (gethash n (board-number->extraction self) -1))
        (with never-win = nil)
        (maximize e into row-winner)
        ; a row win when the last (maximum) number is extracted
        (after-each
          (if (= e -1)
            (setf never-win t)
            (when add-to-content? (vector-push-extend e (board-content self)))))
        (finally
           (unless never-win
             (setf (board-winner-extraction self) (nil-min2 row-winner (board-winner-extraction self))))
           nil)))
             ; the board win at the first (minimum) winning row

(defun* (parse-board! -> boolean)  ((self board) (in stream))
  "Start with a blank line and then complete the board. Return nil if there is no board to parse."
  (iter (for rs in-stream in using #'read-line)
        (for row = (map 'list #'parse-integer (str:split " " rs :omit-nulls t)))
        (for curr-row from 0)
        (for is-there-board? initially nil then t)
        (with cols = nil)
        (until (null row))
        (if-first-time
          (let ((d (length row)))
            (setf cols (make-array (list d d) :element-type 'fixnum))))
        (after-each
           (board-add-row! self row)
           (iter (for n in-sequence row)
                 (for curr-col from 0)
                 (setf (aref cols curr-col curr-row) n)))
        (finally
          (when cols
            (let+ (((col-i _) (array-dimensions cols)))
               (iter (for i from 0 below col-i)
                     (after-each (board-add-row! self (aops:sub cols i) :add-to-content? nil)))))

             (return is-there-board?))))

(defun* (board-winner-number -> (or fixnum null)) ((self board))
  (let* ((we (board-winner-extraction self)))
    (if (null we)
        nil
        (aref (board-extraction->number self) we))))

(defun* (board-score -> fixnum) ((self board))
  "Calculate the score according the rule of the exsercise."
  (iter (for e in-sequence (board-content self))
        (for n = (aref (board-extraction->number self) e))
        (with we = (board-winner-extraction self))
        (with wn = (board-winner-number self))
        (for mn = (if (null we)
                      n  ; TODO probably not correct: should take the last extracted number
                      (if (<= e we)
                          0 ; if the extraction is before the winner
                          n ; number not yet extracted
                      )))
        (sum mn into smn)
        (finally (return (* smn wn)))))

(defun select-board (&key select-best?)
  (let ((in *standard-input*))
    (let+ (((ne en) (parse-board-extractions in)))

      (iter (for b = (make-instance 'board :number->exctraction ne :extraction->number en))
            (for b? = (parse-board! b in))
            (with best-b = nil)
            (with best-extraction = nil)
            (while b?)
            (for b-extraction = (board-winner-extraction b))
            (after-each
               (let ((is-best? (if select-best?
                                   (nil<= b-extraction best-extraction)
                                   (nil>= b-extraction best-extraction))))
                  (when is-best?
                     (setf best-extraction b-extraction)
                     (setf best-b b))))
            (finally (return (board-score best-b)))))))

(defun day4b () (select-board :select-best? nil))

(defun main () (format t "~a~%" (day4b)))
