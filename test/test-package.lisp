;;;; Test package for TRIVIAL-DOCUMENTATION.

(defpackage trivial-documentation.test-package
  (:documentation
   "Test package for TRIVIAL-DOCUMENTATION.")
  (:use :cl)
  (:export :*variable*
	   :+constant+
	   :function-defn
	   :generic-function-defn
	   :macro-defn
	   :setf-defn
	   :class-a-defn
	   :class-b-defn
	   :class-c-defn
	   :structure-defn
	   :type-defn))

(in-package :trivial-documentation.test-package)

(defparameter *variable* 1 "variable")

(defconstant +constant+ 2 "constant")

(defun function-defn (foo &rest bar)
  "function"
  (list foo bar))

(defgeneric generic-function-defn (foo &optional bar)
  (:documentation "generic-function"))

(defmacro macro-defn (foo &body body)
  "macro"
  `(,foo ,@body))

(defun (setf setf-defn) (x n v)
  "setf"
  (setf (car (nthcdr (1- n) x)) v))

(defclass class-a-defn ()
  ((foo :initarg :foo :initform 3))
  (:documentation "class-a"))

(defclass class-b-defn ()
  ((bar :initarg :bar  :initform 4))
  (:documentation "class-b"))

(defclass class-c-defn (class-a-defn class-b-defn)
  ((baz :initarg :boom :initform 5))
  (:documentation "class-c"))

(defstruct structure-defn
  "structure"
  (foo 0))

(deftype type-defn () "type" 'integer)

