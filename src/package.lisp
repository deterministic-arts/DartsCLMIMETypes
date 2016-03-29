#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- MIME Types
  Copyright (c) 2016 Dirk Esser

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

(defpackage "DARTS.LIB.MIME-TYPE"
  (:use "COMMON-LISP" "CL-PPCRE")
  (:export "MIME-PARSE-ERROR" "MIME-PARSE-ERROR-STRING" "MIME-PARSE-ERROR-START"
           "MIME-PARSE-ERROR-END" "MIME-PARSE-ERROR-POSITION" "MIME-PARSE-ERROR-DECODED-FORMAT"
           "MIME-PARSE-ERROR-DECODED-VARIANT" "MIME-PARSE-ERROR-DECODED-PARAMETERS"
           "SIMPLE-MIME-PARSE-ERROR" "PARSE-MIME-TYPE-1" "FORMAT-MIME-TYPE-1"
           "MIME-TYPE" "MIME-TYPE-FORMAT" "MIME-TYPE-VARIANT" "MIME-TYPE-PARAMETERS"
           "MIME-TYPE-STRING" "MIME-TYPE-MEDIA-TYPE" "FORMAT-MIME-TYPE" "PARSE-MIME-TYPE"
           "MIME-TYPE-PARAMETER" "MIME-TYPE-DESIGNATOR" "MIME-TYPE-EQUAL" "MIME-TYPE-HASH"
           "MIME-TYPE-FILE-EXTENSIONS" "MAP-OVER-MIME-FILE-ASSOCIATIONS-1" 
           "LOAD-MIME-FILE-ASSOCIATIONS" "MAP-OVER-MIME-FILE-ASSOCIATIONS")
  (:documentation "A small library, which parses MIME type specifications
    according to the rules of RFC 2616. Full support for MIME type parameters
    is provided."))
