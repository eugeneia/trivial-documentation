;;;; Extract documentation and definitions for symbols and packages.

(defpackage :trivial-documentation
  (:documentation
   "Extract documentation and definitions for symbols and packages.")
  (:use :cl)
  (:export :symbol-definitions
           :package-api))

(in-package :trivial-documentation)

(defun make-definition (kind &rest plist)
  "Base definition constructor. Prepend PLIST with :KIND property of
value KIND."
  (list* :kind kind plist))

(defun variable-definition (variable-name)
  "Compile definition for VARIABLE."
  (make-definition (if (constantp variable-name) :constant :variable)
		   :value (symbol-value variable-name)
		   :documentation (documentation variable-name
						 'variable)))

(defun function-p (function-name)
  "Tests if FUNCTION-NAME is bound to a function."
  (not (null (ignore-errors (fdefinition function-name)))))

(defun macro-function-p (function-name)
  "Tests if FUNCTION-NAME is bound to a macro function."
  (and (symbolp function-name)
       (not (null (macro-function function-name)))))

(defun generic-function-p (function-name)
  "Tests if FUNCTION-NAME is bound to a generic function."
  (typep (fdefinition function-name) 'standard-generic-function))

(defun normalize-lambda-list (lambda-list)
  "Flatten symbols with default values."
  (loop for head = lambda-list then (cdr head)
        for o = (car head)
        for with-defaults = (or (eq o '&key) (eq o '&optional))
     while head
     if (listp o) collect (normalize-lambda-list o) else collect o
     when with-defaults
     append (mapcar (lambda (o)
                      (typecase o
                        (list (first o))
                        (otherwise o)))
                    (cdr head))
     until with-defaults))

(defun lambda-list (function-name)
  "Get lambda list for FUNCTION-NAME."
  (let ((function (if (macro-function-p function-name)
		      (macro-function function-name)
		      (fdefinition function-name))))
    #+openmcl (declare (ignore function))
    (normalize-lambda-list
     #-(or openmcl
           lispworks
           sbcl)
     (second (function-lambda-expression function))
     #+openmcl        (ccl:arglist function-name)
    #+lispworks      (lw:function-lambda-list function)
    #+sbcl (sb-introspect:function-lambda-list function))))

(defun function-definition (function-name)
  "Compile definition for FUNCTION-NAME."
  (make-definition
   (cond ((macro-function-p function-name)   :macro)
         ((generic-function-p function-name) :generic-function)
         (t                                  :function))
   :lambda-list (lambda-list function-name)
   :documentation (documentation function-name 'function)))

(defun class-precedence-list (class-name)
  "Get class precedence list for CLASS-NAME."
  (closer-mop:class-precedence-list (find-class class-name)))

(defun class-slot-initargs (class-name)
  "Get class slot initargs for CLASS-NAME."
  (let ((class (find-class class-name)))
    (mapcan (lambda (slot)
	      (copy-seq (closer-mop:slot-definition-initargs slot)))
            (closer-mop:class-slots class))))

(defun class-definition (class-name)
  "Compile definition for CLASS-NAME."
  (if (subtypep (find-class class-name)
		(find-class 'structure-object))
      (make-definition :structure
		       :documentation (documentation class-name 'type))
      (make-definition :class
		       :precedence-list
		       (mapcar #'class-name
			       (class-precedence-list class-name))
		       :initargs (class-slot-initargs class-name)
		       :documentation (documentation class-name 'type))))

(defun type-definition (type-name)
  "Compile definition for TYPE-NAME."
  (make-definition :type :documentation (documentation type-name 'type)))

(defun symbol-definitions (symbol)
  "*Arguments and Values:*

   _symbol_—a symbol.

   *Description:*

   {symbol-definitions} compiles and returns a list of definitions for
   _symbol_. Each definition is a _property list_ containing at least two
   _properties_:

   + {:kind}—one of {:constant}, {:variable}, {:function},
     {:generic-function}, {:macro}, {:structure}, {:class} or {:type}.
   + {:documentation}—the respective documentation string or {nil}.

   Definitions of kind {:constant} and {:variable} have an additional
   _property_ {:value} which holds the initial value of the constant or
   variable.

   Definitions of kind {:function}, {:generic-function} and {:macro} have
   an additional _property_ {:lambda-list} which holds the lambda list of
   the (generic) function or macro.

   Definitions of kind {:class} have two additional _properties_
   {:precedence-list} and {:initargs} which hold the class precedence
   list and initialization arguments of the class."
  (let (definitions)
    ;; variable/constant?
    (when (boundp symbol)
      (push (variable-definition symbol) definitions))
    ;; function?
    (when (and (fboundp symbol))
      (push (function-definition symbol) definitions))
    ;; setfable?
    (let ((setf-name `(setf ,symbol)))
      (when (function-p setf-name)
	(push (function-definition setf-name) definitions)))
    ;; class?
    (when (ignore-errors (find-class symbol))
      (push (class-definition symbol) definitions))
    ;; type?
    (when (and (documentation symbol 'type)
	       (not (ignore-errors (find-class symbol))))
      (push (type-definition symbol) definitions))
    (nreverse definitions)))

(defun symbol-name-< (symbol-x symbol-y)
  "Predicate to test if symbol names of SYMBOL-X and SYMBOL-Y satisfy
STRING<."
  (string< (symbol-name symbol-x)
           (symbol-name symbol-y)))

(defun package-api (package)
  "*Arguments and Values:*

   _package_—a _package_ or a _string designator_ naming a _package_.

   *Description:*

   {extract-api} compiles and returns a _property list_ mapping the
   external symbols of _package_ to lists of definitions as returned by
   {symbol-definitions}. The returned _property list_ is in alphabetical
   order (by comparing the keys)."
  (values
   (documentation (find-package package) t)
   (loop for symbol in
        (sort (loop for symbol being the external-symbols in package
                 collect symbol)
              #'symbol-name-<)
      collect symbol
      collect (symbol-definitions symbol))))
