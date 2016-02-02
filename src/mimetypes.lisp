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

(in-package "DARTS.LIB.MIME-TYPE")



(define-condition mime-parse-error (parse-error)
  ((string
     :initarg :string :reader mime-parse-error-string
     :documentation "The string being parsed")
   (start
     :initarg :start :initform 0 :reader mime-parse-error-start
     :documentation "Start index of the first character to be parsed")
   (end
     :initarg :end :initform nil :reader mime-parse-error-end
     :documentation "Index of the first character after the part to parsed")
   (position
     :initarg :position :initform nil :reader mime-parse-error-position
     :documentation "Last valid position in the input string")
   (decoded-format 
     :initarg :decoded-format :initform nil 
     :reader mime-parse-error-decoded-format)
   (decoded-variant 
     :initarg :decoded-variant :initform nil 
     :reader mime-parse-error-decoded-variant)
   (decoded-parameters 
     :initarg :decoded-parameters :initform nil 
     :reader mime-parse-error-decoded-parameters))
  (:report (lambda (object stream)
             (let* ((string (mime-parse-error-string object))
                    (start (mime-parse-error-start object))
                    (end (or (mime-parse-error-end object) (length string)))
                    (position (mime-parse-error-position object)))
               (format stream "could not parse ~S as MIME type specification~@[; parsing should have stopped at index ~D~]"
                       (subseq string start end) position))))
  (:documentation "Condition signalled by `parse-mime-type', if the input
    string cannot be parsed as MIME type specification. Note, that `position' 
    slot does not hold the position, where the error was detected, but the
    last valid index into the string, at which parsing could have \"legally\"
    stopped (FIXME: how to explain this better)"))

(define-condition simple-mime-parse-error (simple-condition mime-parse-error) ())



(defun make-mime-type-lexer (string start end)
  (labels
      ((whitep (char) 
         (find char #.(concatenate 'string '(#\space #\tab #\newline #\return))))
       (atom-char-p (char)
         (or (eql char #\!) (char<= #\# char #\')
             (char<= #\* char #\+) (eql char #\.)
             (char<= #\0 char #\9) (char<= #\A char #\Z)
             (char<= #\^ char #\~) (eql char #\-)))
       (read-atom ()
         (loop
            :with first := start
            :while (< start end)
            :do (let ((char (char string start)))
                  (if (atom-char-p char)
                      (incf start)
                      (return (values :atom (coerce (subseq string first start) 'simple-string)
                                      start))))
            :finally (return (values :atom (coerce (subseq string first) 'simple-string)
                                     start))))
       (read-quoted ()
         (loop
            :with buffer := (make-array (- end start) :element-type 'character :fill-pointer 0)
            :and escaped := nil :and string-start := (- start 1)
            :while (< start end)
            :do (let ((char (char string start)))
                  (if escaped
                      (progn
                        (incf start)
                        (vector-push-extend char buffer)
                        (setf escaped nil))
                      (cond
                        ((eql char #\\) (incf start) (setf escaped t))
                        ((eql char #\") (incf start) (return (values :quoted (coerce buffer 'simple-string) start)))
                        (t (incf start)
                           (vector-push-extend char buffer)))))
            :finally (return (values :error string-start string-start)))))
    (lambda ()
      (block scan
        (loop
          (if (>= start end) 
              (return-from scan (values nil nil start))
              (let ((char (char string start)))
                (cond
                  ((whitep char) (incf start))
                  ((atom-char-p char) (return-from scan (read-atom)))
                  ((eql char #\") (incf start) (return-from scan (read-quoted)))
                  ((eql char #\=) (incf start) (return-from scan (values :equal nil start)))
                  ((eql char #\;) (incf start) (return-from scan (values :semicolon nil start)))
                  ((eql char #\/) (incf start) (return-from scan (values :slash nil start)))
                  ((eql char #\,) (incf start) (return-from scan (values :comma nil start)))
                  (t (return-from scan (values :error start start)))))))))))


(defun parse-mime-type-1 (string 
                          &key (start 0) (end nil) (junk-allowed nil))
  (let* ((end (or end (length string)))
         (lexer (make-mime-type-lexer string start end)))
    (labels
        ((next-token () (funcall lexer))
         (fail (after format variant params &optional ctl &rest args)
           (declare (dynamic-extent args))
           (let* ((params (nreverse params))
                  (condition (if (not ctl)
                                (make-condition 'mime-parse-error :string string :start start :end end 
                                                                  :position after :decoded-format format
                                                                  :decoded-variant variant 
                                                                  :decoded-parameters params)
                                (make-condition 'simple-mime-parse-error :string string :start start :end end 
                                                                         :position after :decoded-format format
                                                                         :decoded-variant variant 
                                                                         :decoded-parameters params
                                                                         :format-control ctl 
                                                                         :format-arguments (copy-list args)))))
             (if (not junk-allowed)
                 (error condition)
                 (return-from parse-mime-type-1 (values format variant params condition)))))
         (parse-media-type (after-1)
           (multiple-value-bind (token format after) (next-token)
             (declare (ignore after))
             (if (not (eq token :atom))
                 (fail after-1 nil nil nil "media type format expected")
                 (multiple-value-bind (token unused after) (next-token)
                   (declare (ignore unused after))
                   (if (not (eq token :slash))
                       (fail after-1 format nil nil "missing ~C separator after the media type format ~S" #\/ format)
                       (multiple-value-bind (token variant after-2) (next-token)
                         (if (not (eq token :atom))
                             (fail after-1 format nil nil "missing format variant after ~C separator" #\/)
                             (parse-parameters after-2 format variant nil))))))))
         (parse-parameters (after-1 format variant parameters)
           (multiple-value-bind (token unused) (next-token)
             (declare (ignore unused))
             (cond
               ((null token) (values format variant (nreverse parameters) nil))
               ((not (eq token :semicolon)) (fail after-1 format variant parameters "junk found after parameter list"))
               (t (multiple-value-bind (token key) (next-token)
                    (cond
                      ((null token) (values format variant (nreverse parameters) nil))
                      ((not (eq token :atom)) (fail after-1 format variant parameters "parameter name expected; found ~S instead" token))
                      (t (multiple-value-bind (token unused after) (next-token)
                           (declare (ignore unused after))
                           (if (not (eq token :equal))
                               (fail after-1 format variant parameters "expected ~C, but found ~S instead" #\= token)
                               (multiple-value-bind (token value after-2) (next-token)
                                 (if (not (or (eq token :atom) (eq token :quoted)))
                                     (fail after-1 format variant parameters "expected the parameter value for ~S, but got ~S instead" key token)
                                     (parse-parameters after-2 format variant 
                                                       (cons (cons key value) 
                                                             parameters))))))))))))))
      (parse-media-type start))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun simple-mime-token-p (string)
    (and (stringp string)
         (scan "^[!#-'*-+.0-9A-Z^-~-]+$" string)
         t)))

(deftype simple-mime-token ()
  '(and string (satisfies simple-mime-token-p)))

(defun simple-mime-token (value &key (case-mode :preserve))
  (labels
      ((from-string (string)
         (if (simple-mime-token-p string)
             (ecase case-mode
               ((:preserve) string)
               ((:upcase) (string-upcase string))
               ((:downcase) (string-downcase string)))
             (error 'simple-type-error
                    :datum value :expected-type 'simple-mime-token
                    :format-control "~S is not a well-formed simple MIME token"
                    :format-arguments (list value)))))
    (cond
      ((stringp value) (from-string value))
      ((symbolp value) (from-string (symbol-name value)))
      ((characterp value) (from-string (string value)))
      (t (error 'simple-type-error
                :datum value :expected-type 'simple-mime-token
                :format-control "~S is not a well-formed simple MIME token"
                :format-arguments (list value))))))


(defun format-mime-type-1 (format variant parameters 
                           &key (case-mode :preserve) (stream *standard-output*) (dense t))
  (write-string (simple-mime-token format :case-mode case-mode) stream)
  (write-char #\/ stream)
  (write-string (simple-mime-token variant :case-mode case-mode) stream)
  (dolist (pair parameters)
    (write-char #\; stream)
    (unless dense (write-char #\space stream))
    (write-string (simple-mime-token (car pair) :case-mode case-mode) stream)
    (write-char #\= stream)
    (let ((value (string (cdr pair))))
      (if (simple-mime-token-p value)
          (write-string value stream)
          (progn
            (write-char #\" stream)
            (loop
              :for char :across value
              :do (when (eql char #\") (write-char #\\ stream))
                  (write-char char stream))
            (write-char #\" stream)))))
  nil)



(defgeneric mime-type-format (object))
(defgeneric mime-type-variant (object))
(defgeneric mime-type-parameters (object))
(defgeneric mime-type-string (object))
(defgeneric mime-type-media-type (object))
(defgeneric mime-type-base-type (object))

(defclass mime-type ()
  ((format
     :type simple-mime-token
     :reader mime-type-format)
   (variant
     :type simple-mime-token :initform "*"
     :reader mime-type-variant)
   (parameters
     :type list :initform nil
     :reader mime-type-parameters)
   (media-type
     :type string
     :reader mime-type-media-type)
   (string
     :type string
     :reader mime-type-string))
  (:documentation "Instances of this class represent fully parsed
    MIME type strings. Each MIME type has media type, which is composed
    of a primary `format' and a `variant', as well as an optional list
    of parameters."))

(defmethod slot-unbound (class (object mime-type) (slot (eql 'string)))
  (declare (ignore class slot))
  (setf (slot-value object 'string)
        (with-output-to-string (stream)
          (format-mime-type-1 (mime-type-format object) (mime-type-variant object) (mime-type-parameters object)
                              :stream stream :dense nil))))

(defmethod slot-unbound (class (object mime-type) (slot (eql 'media-type)))
  (declare (ignore class slot))
  (setf (slot-value object 'media-type)
        (concatenate 'string (mime-type-format object) "/" (mime-type-variant object))))


(defmethod shared-initialize :after ((object mime-type) slots 
                                     &key (format nil have-format) (variant nil have-variant) 
                                          (parameters nil have-parameters))
  (slot-makunbound object 'media-type)
  (slot-makunbound object 'string)
  (when have-format (setf (slot-value object 'format) (simple-mime-token format :case-mode :downcase)))
  (when have-variant (setf (slot-value object 'variant) (simple-mime-token variant :case-mode :downcase)))
  (when have-parameters
    (let ((list (mapcar (lambda (pair) (cons (simple-mime-token (car pair) :case-mode :downcase) (string (cdr pair))))
                        parameters)))
      (setf (slot-value object 'parameters)
            (stable-sort list #'string-lessp :key #'car)))))
        

(defmethod print-object ((object mime-type) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A/~A~{ ~A~}"
            (mime-type-format object)
            (mime-type-variant object)
            (mapcar (lambda (pair) (format nil "~A=~S" (car pair) (cdr pair)))
                    (mime-type-parameters object)))))


(defvar +root-type+
  (make-instance 'mime-type
                 :format "*" :variant "*" :parameters nil))

(defvar *base-type-map* 
  (make-hash-table :test 'equal))

(loop
  :for format :in '("text" "image" "video" "audio" "application")
  :do (setf (gethash format *base-type-map*) (make-instance 'mime-type :format format :variant "*")))


(defmethod mime-type-base-type ((object mime-type))
  (cond
    ((mime-type-parameters object) (make-instance 'mime-type :format (mime-type-format object) :variant (mime-type-variant object)))
    ((string= "*" (mime-type-variant object)) +root-type+)
    (t (let* ((format (mime-type-format object))
              (present (gethash format *base-type-map*)))
         (or present
             (if (string= "*" format) object
                 (make-instance 'mime-type 
                                :format format 
                                :variant "*")))))))

(defun parse-mime-type (string &key (start 0) end (junk-allowed nil))
  (multiple-value-bind (format variant parameters condition) (parse-mime-type-1 string :start start :end end :junk-allowed junk-allowed)
    (if condition
        (values nil condition)
        (values (make-instance 'mime-type :format format :variant variant :parameters parameters)
                nil))))


(defun format-mime-type (object &key (stream *standard-output*) (dense nil))
  (if (and (not dense) (slot-boundp object 'string))
      (write-string (mime-type-string object) stream)
      (format-mime-type-1 (mime-type-format object) (mime-type-variant object) (mime-type-parameters object)
                          :stream stream :dense dense))
  object)


(deftype mime-type-designator ()
  '(or string mime-type))

(defun mime-type (object)
  (typecase object
    (mime-type object)
    (string (parse-mime-type object))
    (symbol (parse-mime-type (symbol-name object)))
    (character (parse-mime-type (string object)))
    (t (error 'simple-type-error
              :datum object :expected-type 'mime-type-designator
              :format-control "~S is not a valid MIME type designator"
              :format-arguments (list object)))))


(defun mime-type-equal (type1 type2 &key (test #'string=))
  "Compares the MIME type values `type1' and `type2', answering true,
   if both are equal. By definition, two MIME type descriptions are
   equal, if the format and variant fields are equal, and if both have
   equal parameter lists. The given `test' function is used to compare
   the parameter values, and it defaults to `string=' (i.e., taking
   letter case into account). Depending on the use case, an application
   may want to pass in `string-equal' instead."
  (let ((type1 (mime-type type1))
        (type2 (mime-type type2)))
    (and (string= (mime-type-format type1) (mime-type-format type2))
         (string= (mime-type-variant type1) (mime-type-variant type2))
         (labels
             ((recurse (p1 p2)
                (cond
                  ((null p1) (null p2))
                  ((null p2) nil)
                  ((not (string= (caar p1) (caar p2))) nil)
                  ((not (funcall test (cdar p1) (cdar p2))) nil)
                  (t (recurse (cdr p1) (cdr p2))))))
           (recurse (mime-type-parameters type1) 
                    (mime-type-parameters type2))))))


(defun mime-type-hash (object)
  (let ((object (mime-type object)))
    (logxor (sxhash (mime-type-format object))
            (sxhash (mime-type-variant object)))))



;;; The following methods could all be implemented by parsing
;;; the input string first, and calling the appropriate reader
;;; on the resulting object. But since parsing is actually somewhat
;;; expensive, we try to avoid it, if we have a simpler way to 
;;; extract the requested piece of information
;;;
;;; XXX: This might be a case of misguided or premature optimisation,
;;; since I never measured, whether the regex based approach is
;;; really cheaper than employing the full parser.


(defmethod mime-type-format ((object string))
  (multiple-value-bind (match groups) (scan-to-strings "^\\s*([!#-'*-+.0-9A-Z^-~-]+)\\s*/\\s*[!#-'*-+.0-9A-Z^-~-]+\\s*$" object)
    (if match
        (string-downcase (aref groups 0))
        (mime-type-format (parse-mime-type object)))))

(defmethod mime-type-variant ((object string))
  (multiple-value-bind (match groups) (scan-to-strings "^\\s*[!#-'*-+.0-9A-Z^-~-]+\\s*/\\s*([!#-'*-+.0-9A-Z^-~-]+)\\s*$" object)
    (if match
        (string-downcase (aref groups 0))
        (mime-type-variant (parse-mime-type object)))))

(defmethod mime-type-media-type ((object string))
  (multiple-value-bind (match groups) (scan-to-strings "^\\s*([!#-'*-+.0-9A-Z^-~-]+)\\s*/\\s*([!#-'*-+.0-9A-Z^-~-]+)\\s*$" object)
    (if match
        (concatenate 'string (string-downcase (aref groups 0)) "/" (string-downcase (aref groups 1)))
        (mime-type-media-type (parse-mime-type object)))))

(defmethod mime-type-base-type ((object string))
  (multiple-value-bind (match groups) (scan-to-strings "^\\s*([!#-'*-+.0-9A-Z^-~-]+)\\s*/\\s*([!#-'*-+.0-9A-Z^-~-]+)\\s*$" object)
    (if (not match) 
        (mime-type-base-type (parse-mime-type object))
        (let ((format (aref groups 0))
              (variant (aref groups 1)))
          (if (string= "*" variant) +root-type+
              (let ((object (gethash format *base-type-map*)))
                (or object
                    (if (string= format "*") +root-type+
                        (make-instance 'mime-type :format format :variant "*")))))))))
        

(defmethod mime-type-string ((object string))
  (mime-type-string (parse-mime-type object)))

(defmethod mime-type-parameters ((object string))
  (mime-type-parameters (parse-mime-type object)))


(defun mime-type-parameter (object name &optional default)
  (let ((pair (assoc name (mime-type-parameters object) :test #'string-equal)))
    (if pair
        (values (cdr pair) t)
        (values default nil))))

