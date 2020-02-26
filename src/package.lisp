#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- MIME Types
  Copyright (c) 2016, 2020 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(defpackage #:darts.lib.mime-type
  (:use #:common-lisp #:cl-ppcre)
  (:export #:mime-parse-error #:mime-parse-error-string #:mime-parse-error-start
           #:mime-parse-error-end #:mime-parse-error-position #:mime-parse-error-decoded-format
           #:mime-parse-error-decoded-variant #:mime-parse-error-decoded-parameters
           #:simple-mime-parse-error #:parse-mime-type-1 #:format-mime-type-1
           #:mime-type #:mime-type-format #:mime-type-variant #:mime-type-parameters
           #:mime-type-string #:mime-type-media-type #:format-mime-type #:parse-mime-type
           #:mime-type-parameter #:mime-type-designator #:mime-type-equal #:mime-type-hash
           #:mime-type-file-extensions #:map-over-mime-file-associations-1 
           #:load-mime-file-associations #:map-over-mime-file-associations #:basic-mime-type
           #:standard-mime-type)
  (:documentation "A small library, which parses MIME type specifications
    according to the rules of RFC 2616. Full support for MIME type parameters
    is provided."))
