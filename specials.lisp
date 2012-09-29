;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package:CL-WHO; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-who/specials.lisp,v 1.6 2009/01/26 11:10:49 edi Exp $

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

#+:sbcl
(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;;; http://www.w3.org/QA/2002/04/valid-dtd-list.html
;;
;; Some SGML prologues
(defconstant        html4-strict "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")
(defconstant  html4-transitional "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
(defconstant      html4-frameset "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">")
(defconstant               htnl5 "<!DOCTYPE HTML>")
(defvar *sgml-prologues*
  '(html4-strict html4-transitional html4-frameset htnl5))
;;
;; Some XML prologues
(defconstant                xml1 "<?xml version='1.0' encoding='UTF-8'?>")
(defconstant       xhtml1-strict "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
(defconstant xhtml1-transitional "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
(defconstant     xhtml1-frameset "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">")
(defconstant              xhtml5 "<?xml version='1.0' encoding='UTF-8'?>")
(defvar *xml-prologues*
  '(xml1 xhtml1-strict xhtml1-transitional xhtml1-frameset xhtml5))
;;
;; http://dev.w3.org/html5/html4-differences/#syntax
;;
;; XHTML5 must be served with MIME type 'application/xhtml+xml' and the html
;; element must include the following namespace declaration:
;;
;;  <html xmlns='http://www.w3.org/1999/xhtml'>
;;   ...
;;  </html>

(defvar *known-prologues* (append *sgml-prologues* *xml-prologues*))

(defvar *prologue* 'xhtml1-strict
  "The default prologue, i.e., the first line printed by
WITH-HTML-OUTPUT[-TO-STRING] when the :PROLOGUE keyword argument is T.

Default value:  XHTML1-STRICT

The convenience function (SETF (PROLOGUE) ...) checks the new value against
*KNOWN-PROLOGUES* before binding this variable.  If a variety of document types
are required, supplying a :PROLOGUE keyword argument is your only option, i.e.,
lexically binding this variable will not have the desired effect.")

(defvar *indent* nil
  "Whether to insert line breaks and indent. Also controls amount of
indentation dynamically.")

(defvar *downcase-tokens-p* t
  "If NIL, a keyword symbol representing a tag or attribute name will
not be automatically converted to lowercase.  This is useful when one
needs to output case sensitive XML.")

(defvar *attribute-quote-char* #\'
  "Quote character for attributes.")

(defvar *html-no-indent-tags*
  '(:pre :textarea)
  "The list of HTML tags that should disable indentation inside them. The initial
value is a list containing only :PRE and :TEXTAREA.")

(defvar *pre-html5-void-elements*
  '(:area
    :atop
    :audioscope
    :base
    :basefont
    :br
    :choose
    :col
    :command
    :embed
    :frame
    :hr
    :img
    :input
    :isindex
    :keygen
    :left
    :limittext
    :link
    :meta
    :nextid
    :of
    :over
    :param
    :range
    :right
    :source
    :spacer
    :spot
    :tab
    :track
    :wbr)
  "The list of HTML tags that should be output as empty tags.
See *HTML-VOID-ELEMENTS-AWARE-P*.")

(defvar *html-void-elements-aware-p* t
  "Set this to a non-NIL value if you want the empty tags listed in
*PRE-HTML5-VOID-ELEMENTS* to be written \"<tag>\" \(SGML syntax) or
\"<tag \>\" \(XML syntax).  \(All other tags are written \"<tag></tag>\").
This is the default.

Set this to NIL if you want all tags to be written \"<tag></tag>\" regardless as
to whether or not they are listed in *PRE-HTML5-VOID-ELEMENTS*.  This is useful
if you want CL-WHO to behave as a strict XML generator.")

(defconstant +newline+
  (make-string 1 :initial-element #\Newline)
  "Used for indentation.")

(defconstant +spaces+
  (make-string 2000 :initial-element #\Space :element-type 'base-char)
  "Used for indentation.")

;;; define these as special variables in order to make it easy to change the
;;; behaviour of ESCAPE-STRING (and the functions in convenience.lisp)
(defvar *escape-char-test* 'escape-char-p
  "Used by ESCAPE-STRING to test whether a character should be escaped.")

(defvar *escape-char-function* 'escape-char
  "Used by ESCAPE-STRING to perform the actual escaping.")
