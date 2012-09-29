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

(defun minimal-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-MINIMAL functions to determine
whether CHAR must be escaped."
  (find char "<>&"))

(defun escape-char-minimal (char)
  "Escapes only #\<, #\>, and #\& characters."
  (funcall *escape-char-function* char :test #'minimal-escape-char-p))

(defun escape-string-minimal (string)
  "Escapes only #\<, #\>, and #\& in STRING."
  (escape-string string :test #'minimal-escape-char-p))

(defun minimal-plus-quotes-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-MINIMAL-PLUS-QUOTES functions to
determine whether CHAR must be escaped."
  (find char "<>&'\""))

(defun escape-char-minimal-plus-quotes (char)
  "Like ESCAPE-CHAR-MINIMAL but also escapes quotes."
  (funcall *escape-char-function* char
           :test #'minimal-plus-quotes-escape-char-p))

(defun escape-string-minimal-plus-quotes (string)
  "Like ESCAPE-STRING-MINIMAL but also escapes quotes."
  (escape-string string :test #'minimal-plus-quotes-escape-char-p))

(defun iso-8859-1-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-ISO-8859-1 functions to
determine whether CHAR must be escaped."
  (or (find char "<>&'\"")
      (> (char-code char) 255)))

(defun escape-char-iso-8859-1 (char)
  "Escapes characters that aren't defined in ISO-8859-9."
  (funcall *escape-char-function* char
           :test #'iso-8859-1-escape-char-p))

(defun escape-string-iso-8859-1 (string)
  "Escapes all characters in STRING which aren't defined in ISO-8859-1."
  (escape-string string :test #'iso-8859-1-escape-char-p))

(defun non-7bit-ascii-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-ISO-8859-1 functions to
determine whether CHAR must be escaped."
  (or (find char "<>&'\"")
      (> (char-code char) 127)))

(defun escape-char-all (char)
  "Escapes characters which aren't in the 7-bit ASCII character set."
  (funcall *escape-char-function* char
           :test #'non-7bit-ascii-escape-char-p))

(defun escape-string-all (string)
  "Escapes all characters in STRING which aren't in the 7-bit ASCII
character set."
  (escape-string string :test #'non-7bit-ascii-escape-char-p))
