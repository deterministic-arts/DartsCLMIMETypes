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

(defgeneric mime-type-format (object)
  (:documentation "Answers the general format name of the media type
    being represented by the given MIME type designator `object'. The result
    is a string, such as `application', `image', `multipart', etc.
    Conventionally, the string returned by this function contains only
    lower-case characters. Applications, which define methods on this 
    function for their own specific representations should follow this 
    convention in order to maximize interoperability."))

(defgeneric mime-type-variant (object)
  (:documentation "Answers the format variant name of the media type
    being represented by the given MIME type designator `object'. The result
    is a string, such as `octet-stream', `png', `vnd.ms-excel', etc.
    Conventionally, the string returned by this function contains only
    lower-case characters. Applications, which define methods on this 
    function for their own specific representations should follow this 
    convention in order to maximize interoperability."))

(defgeneric mime-type-parameters (object)
  (:documentation "Answers the list of parameters associated with the
    MIME type designator `object'. The list is an alist, whose keys and
    values are strings. The keys are usually compared case-insensitively.
    Note, that this library requires, that the parameters are sorted
    in lexicographic order of their keys.

    Note: the parser provided by this library normalizes the case of
    parameter names to lower-case during parsing, and most code in 
    this library assumes, that parameter names have been canonicalized
    this way. Applications, which define methods on this function for
    their own specific representations should follow this convention in
    order to maximize interoperability."))

(defgeneric mime-type-string (object)
  (:documentation "Answers a string representation of the MIME type
    designator `object'. The resulting string is syntactically well-formed
    and can be parsed back via `parse-mime-type'. In particular, all 
    parameter keys and/or values are properly quoted (if they need to
    be)."))

(defgeneric mime-type-media-type (object)
  (:documentation "Answers a string, which represents the media type
    of the given MIME type designator `object'. The result is generally
    a string of the form `format/variant', e.g., `text/plain', `image/png',
    `application/octet-stream', etc."))

(defgeneric mime-type-file-extensions (object))



(defclass mime-type () ()
  (:documentation "Protocol class for MIME types. This class exists
    primarily for type discrimination purposes. All concrete MIME type
    implementations should inherit from this class.

    Instances of this class are supposed to be immutable after construction."))

(defclass basic-mime-type (mime-type)
  ((format
     :type simple-mime-token
     :reader mime-type-format)
   (variant
     :type simple-mime-token :initform "*"
     :reader mime-type-variant)
   (parameters
     :type list :initform nil
     :reader mime-type-parameters))
  (:documentation "Essential MIME type representation. This class 
    provides slots to hold the primary `format' of the type, as well
    as the `variant' and `parameters'. It may be a convenient starting
    point for application-specific MIME type representations."))

(defclass standard-mime-type (basic-mime-type)
  ((file-extensions
     :type list :initarg :file-extensions :initform nil
     :reader mime-type-file-extensions)
   (media-type
     :type string
     :reader mime-type-media-type)
   (string
     :type string
     :reader mime-type-string))
  (:documentation "Instances of this class represent fully parsed
    MIME type strings. Each MIME type has media type, which is composed
    of a primary `format' and a `variant', as well as an optional list
    of parameters. This is the default MIME type representation, which
    is generated by `parse-mime-type' (and related functions), unless a
    specific class is passed in."))

(defmethod mime-type-file-extensions ((object mime-type)) 
  (declare (ignore object)) 
  nil)

(defmethod mime-type-string ((object mime-type))
  (with-output-to-string (stream)
    (format-mime-type-1 (mime-type-format object) (mime-type-variant object) (mime-type-parameters object)
                        :stream stream :dense nil)))

(defmethod mime-type-media-type ((object mime-type))
  (with-output-to-string (stream)
    (format-mime-type-1 (mime-type-format object) (mime-type-variant object) nil
                        :stream stream :dense nil)))

(defmethod slot-unbound (class (object standard-mime-type) (slot (eql 'string)))
  (declare (ignore class slot))
  (setf (slot-value object 'string)
        (with-output-to-string (stream)
          (format-mime-type-1 (mime-type-format object) (mime-type-variant object) (mime-type-parameters object)
                              :stream stream :dense nil))))

(defmethod slot-unbound (class (object standard-mime-type) (slot (eql 'media-type)))
  (declare (ignore class slot))
  (setf (slot-value object 'media-type)
        (let* ((full-type (mime-type-string object))
               (semicolon (position #\; full-type)))
          (if (not semicolon) full-type
              (make-array semicolon 
                          :element-type (array-element-type full-type)
                          :displaced-index-offset 0
                          :displaced-to full-type)))))


(defmethod shared-initialize :after ((object standard-mime-type) slots 
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


(defun parse-mime-type (string &key (start 0) end (junk-allowed nil) (class 'standard-mime-type got-class))
  (multiple-value-bind (format variant parameters condition) (parse-mime-type-1 string :start start :end end :junk-allowed junk-allowed)
    (cond 
      (condition (values nil condition))
      ((not got-class) (values (make-instance 'standard-mime-type :format format :variant variant :parameters parameters) nil))
      (t (values (make-instance class :format format :variant variant :parameters parameters) nil)))))

(defun format-mime-type (object &key (stream *standard-output*) (dense nil))
  (if (and (typep object 'standard-mime-type) (not dense) (slot-boundp object 'string))
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

(defmethod mime-type-string ((object string))
  (mime-type-string (parse-mime-type object)))

(defmethod mime-type-parameters ((object string))
  (mime-type-parameters (parse-mime-type object)))

(defun mime-type-parameter (object name &optional default)
  (let ((pair (assoc name (mime-type-parameters object) :test #'string-equal)))
    (if pair
        (values (cdr pair) t)
        (values default nil))))



(defvar *default-mime-associations-file* #P"/etc/mime.types")


(defun map-over-mime-file-associations-1 (function 
                                          &key (file *default-mime-associations-file*) 
                                               (external-format :default))
  (with-open-file (stream file
                          :element-type 'character
                          :direction :input
                          :external-format external-format)
    (loop
      :for line := (read-line stream nil nil) :while line
      :do (cond
            ((scan "^\\s*#.*" line) nil)
            (t (multiple-value-bind (match groups) (scan-to-strings "^\\s*([!#-'*-+.0-9A-Z^-~-]+)\\s*/\\s*([!#-'*-+.0-9A-Z^-~-]+)\\s+([^\\s]+.*)?$" line)
                 (when match
                   (let ((format (string-downcase (aref groups 0)))
                         (variant (string-downcase (aref groups 1)))
                         (types (and (aref groups 2) (string-trim '(#\space #\tab) (aref groups 2)))))
                     (when (and types (plusp (length types)))
                       (loop
                         :with start := 0 :and extensions := nil
                         :for index :upfrom 0 :below (length types)
                         :for char := (char types index)
                         :when (or (eql char #\space) (eql char #\tab))
                           :do (when (< start index) (push (subseq types start index) extensions))
                               (setf start (1+ index))
                         :finally (when (< start index) (push (subseq types start index) extensions))
                                  (funcall function format variant (nreverse extensions))))))))))))


(defun map-over-mime-file-associations (function 
                                        &key (file *default-mime-associations-file*) 
                                             (external-format :default))
  (map-over-mime-file-associations-1 
    (lambda (format variant extensions)
      (let ((type (make-instance 'standard-mime-type 
                                 :format format :variant variant 
                                 :file-extensions extensions)))
        (funcall function type)))
    :file file 
    :external-format external-format))


(defun load-mime-file-associations (&key (file *default-mime-associations-file*) 
                                         (external-format :default))
  (let ((table (make-hash-table :test 'equal)))
    (map-over-mime-file-associations (lambda (type) 
                                       (dolist (ext (mime-type-file-extensions type))
                                         (setf (gethash ext table) type)))
                                     :file file :external-format external-format)
    table))
