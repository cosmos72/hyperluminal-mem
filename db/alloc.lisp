;; -*- lisp -*-

;; This file is part of hyperluminal-DB.
;; Copyright (c) 2013 Massimiliano Ghilardi
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


(in-package :hyperluminal-db)

(defun mfree-head-index (index)
  "Given index to end-of-magic, return index of free areas head"
  ;; reserve enough space for two words after magic
  (let ((pointer (ceiling (+ 2 index) +mem-box/min-words+)))
    (- (box-pointer->size pointer) +mem-box/header-words+)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mfree* nil "thread-local list of unallocated mmap memory")


(defun init-free-list (ptr index total-n-words)
  "Create and return a new free list containing ALL the words from INDEX up to TOTAL-N-WORDS."
  (declare (type maddress ptr)
           (ignore ptr)
           (type mem-size index total-n-words))
  (let ((lo index)
        (hi (mem-size- total-n-words +mem-box/header-words+)))

    (setf *mfree*
          (make-box lo 0
                    (make-box hi (mem-size- hi lo))))))


(defun mwrite-free-list (ptr free-list)
  "Write a list of free boxes into memory starting at PTR and return it.
FIXME: it currently loads the whole free-list in RAM (bad!)"
  (declare (type maddress ptr))

  (loop for box = free-list then (box-next box)
     while box
     do
       (mwrite-box/free ptr box)))


(defun mread-free-list (ptr index)
  "Read a list of free boxes from memory starting at (PTR + INDEX) and return it.
FIXME: it currently loads the whole free-list in RAM (bad!)"
  (declare (type maddress ptr))

  (let ((head)
        (prev))
    (loop
       (multiple-value-bind (this next-index) (mread-box/free ptr index)
         (declare (type mem-size next-index))

         (if prev
           (setf (box-next prev) this)
           (setf head this
                 ;; head is just a pointer to next box, it must have zero free words
                 (box-n-words this) 0))

         (when (< next-index +mem-box/min-words+)
           (return (setf *mfree* head)))

         (setf prev this
               index next-index)))))


(defun minsert-box/free (ptr prev box lo hi)
  (declare (type maddress ptr)
           (type box prev box)
           (type mem-size lo hi))

  (let ((curr (box-next prev)))
    ;; boxes are written at the end of the free mmap area they represent!
    (setf (box-index   box) hi
          (box-n-words box) (mem-size- hi lo)
          (box-next    box) curr
          (box-next    prev) box)

    (mwrite-box/free ptr box)
    (mwrite-fbox-next ptr prev)
    prev))


(defun %box-free (ptr head box lo hi)
  (declare (type maddress ptr)
           (type box head box)
           (type mem-size lo hi))

  (loop
     for prev = head then curr
     for curr = (box-next prev)
     while curr
     for curr-hi = (box-index curr)
     for curr-n  = (box-n-words curr)
     for curr-lo = (mem-size- curr-hi curr-n)
     do
       (cond
         ((> lo curr-hi)
          nil)
         ((= lo curr-hi)
          (setf lo curr-lo)
          (setf (box-next prev) (box-next curr))
          (mwrite-fbox-next ptr prev)
          (setf curr prev))
         ((= hi curr-lo)
          (setf hi curr-hi)
          (setf (box-next prev) (box-next curr))
          (mwrite-fbox-next ptr prev)
          (setf curr prev))
         ((< hi curr-lo)
          (loop-finish)))
     finally
       (minsert-box/free ptr prev box lo hi)))


(defun box-free (ptr box)
  "A very naive deallocator. Useful only for debugging and development."
  (declare (type maddress ptr)
           (type box box))

  (let ((head *mfree*)
        (n-words (box-n-words box)))
    
    (unless (zerop n-words)
      (let* ((lo (mem-size- (box-index box) +mem-box/header-words+))
             (hi (mem-size+ lo n-words)))
        (%box-free ptr head box lo hi)))

    head))






(defun box-alloc (ptr n-words)
  "A very naive first-fit allocator for mmap areas. Useful only for debugging and development."
  (declare (type maddress ptr)
           (type mem-size n-words))

  ;; trying to allocate zero words? then return invalid pointer
  (when (zerop n-words)
    (return-from box-alloc nil))

  (when (> n-words +mem-box/max-words+)
    (error "cannot allocate ~S consecutive words from mmap area. Maximum supported is ~S words"
            n-words +mem-box/max-words+))

  ;; round up n-words to a multiple of +mem-box/min-words+
  (let ((remainder (logand n-words (1- +mem-box/min-words+))))
    (unless (zerop remainder)
      (incf-mem-size n-words (- +mem-box/min-words+ remainder))))

  (loop for prev = *mfree* then this
     for this = (box-next prev) then next
     while this
     for this-len = (box-n-words this)
     for next = (box-next this)
     do
       (when (>= this-len n-words)
         ;; boxes are written at the end of the free mmap area they represent!
         (let ((result (mem-size- (box-index this) (mem-size- this-len +mem-box/header-words+)))
               (box nil))
           ;; update this length
           (decf this-len n-words)

           (if (zerop this-len)
               ;; exact match? then remove THIS from free list (it cannot be the head)
               (let ((next (box-next this)))
                 (setf (box-next prev) next)
                 ;; write back the new link PREV->NEXT that bypasses THIS
                 (mwrite-fbox-next ptr prev)
                 (setf box this
                       (box-index box) result))

               ;; otherwise update THIS n-words
               (progn
                 (setf (box-n-words this) this-len
                       ;; create and return a new box
                       box (make-box result n-words))
                 (mwrite-fbox-n-words ptr this)))

           (return-from box-alloc box))))

  (error "out of memory! failed to allocate ~S words from mmap area ~S" n-words ptr))

             
                 

(defun box-alloc-rounded (ptr n-words)
  "Round up N-WORDS somewhat (typically 25%) then allocate that many words from mmap area."
  (declare (type maddress ptr)
           (type mem-size n-words))

  (let ((delta
         (if (<= n-words #.(truncate +mem-box/max-words+ 2))
             n-words
             (mem-size- +mem-box/max-words+ n-words))))

    (the (values list &optional)
      (box-alloc ptr (mem-size+ n-words (ash delta 2))))))




(defun box-realloc (ptr box n-words)
  "Extend BOX to N-WORDS if possible, otherwise free it then allocate N-WORDS and return them."
  (declare (type maddress ptr)
           (type (or null box) box)
           (type mem-size n-words))

  ;; very naive implementation: always frees BOX and allocates a new one.
  (when box (box-free ptr box))
  (box-alloc ptr n-words))


