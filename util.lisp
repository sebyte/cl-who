;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package:CL-WHO; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-who/util.lisp,v 1.4 2009/01/26 11:10:49 edi Exp $

;;; Copyright (c) 2003-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-who)

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'lw:with-unique-names))

#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

(defun extract-declarations (forms)
  "Given a FORM, the declarations - if any - will be extracted
   from the head of the FORM, and will return two values the declarations,
   and the remaining of FORM"
  (loop with declarations
        for forms on forms
        for form = (first forms)
        while (and (not (atom form)) ; my fix
                   (eql (first form) 'cl:declare))
        do (push form declarations)
        finally (return (values (nreverse declarations) forms))))

(defun apply-to-tree (function test tree)
  (declare (optimize speed space))
  (declare (type function function test))
  "Applies FUNCTION recursively to all elements of the tree TREE \(not
only leaves) which pass TEST."
  (cond
    ((funcall test tree)
     (funcall function tree))
    ((consp tree)
      (cons
       (apply-to-tree function test (car tree))
       (apply-to-tree function test (cdr tree))))
    (t tree)))

(defmacro n-spaces (n)
  "A string with N spaces - used by indentation."
  `(make-array ,n
               :element-type 'base-char
               :displaced-to +spaces+
               :displaced-index-offset 0))

;;; prologue & syntax
(defun prologue () "Return the current prologue." *prologue*)

(defun (setf prologue) (prologue)
  "Set the cuurent prologue.  Raise an error if PROLOGUE is not included in
*KNOWN-PROLOGUES*."
  (if (not (member prologue *known-prologues*))
      (error "Unknown prologue: ~a" prologue)
    (setf *prologue* prologue)))

(defun prologue-string (prologue)
  (when (eq prologue t) (setq prologue *prologue*))
  (if (not (member prologue *known-prologues*))
      (error "Unknown prologue: ~a" prologue)
    (eval prologue)))

(defun syntax (prologue)
  "Return the appropriate syntax for the given PROLOGUE."
  (when (or (eq prologue t) (not prologue))
    (setq prologue *prologue*))
  (cond ((member prologue *sgml-prologues*) :sgml)
        ((member prologue *xml-prologues*) :xml)
        (t (error "Unknown prologue ~a" prologue))))

(defun void-element-end (syntax)
  "Return the appropriate void element ending for the given SYNTAX."
  (cond ((eq syntax :sgml) ">")
        ((eq syntax :xml) " />")
        (t (error "Unknown syntax ~a" syntax))))

;;; escaping
(defun escape-char-p (char)
  (or (find char "<>&'\"")
      (> (char-code char) 127)))

(declaim (inline escape-char))
(defun escape-char (char syntax &key (test *escape-char-test*))
    (declare (optimize speed))
    (if (funcall test char)
        (case char
          (#\< "&lt;")
          (#\> "&gt;")
          (#\& "&amp;")
          (#\' (if (eq syntax :xml) "&apos;" "&#039;"))
          (#\" "&quot;")
          (t (format nil (if (eq syntax :xml) "&#x~x;" "&#~d;")
                     (char-code char))))
        (make-string 1 :initial-element char)))

(defun escape-string (string syntax
                      &key
                      (test *escape-char-test*)
                      (delegate *escape-char-function*))
  (declare (optimize speed))
  "Escape all characters in STRING which pass TEST.  STRING may be NIL in which
case NIL is returned."
  (let ((first-pos (position-if test string)))
    (if (not first-pos)
        ;; nothing to do, just return STRING
        string
      (with-output-to-string (s)
        (loop with len = (length string)
              for old-pos = 0 then (1+ pos)
              for pos = first-pos
                  then (position-if test string :start old-pos)
              ;; now the characters from OLD-POS to (excluding) POS
              ;; don't have to be escaped while the next character has to
              for char = (and pos (char string pos))
              while pos
              do (write-sequence string s :start old-pos :end pos)
                 (write-sequence (funcall delegate char syntax :test test) s)
              while (< (1+ pos) len)
              finally (unless pos
                        (write-sequence string s :start old-pos)))))))
