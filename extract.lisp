;;;; Extract API from a package.

(in-package :package-api)

(defun keyword-symbol (symbol)
  "Returns SYMBOL interned into the :keyword package."
  (intern (symbol-name symbol) :keyword))

(defun make-definition (kind &rest plist)
  "Prepend PLIST with ((:kind KIND)). Base definition constructor."
  (list* :kind kind plist))

(defun variable-definition (variable-name)
  "Construct definition for VARIABLE."
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

(defun lambda-list (function-name)
  "Return lambda list for FUNCTION."
  (let ((function (if (macro-function-p function-name)
		      (macro-function function-name)
		      (fdefinition function-name))))
    #-(or openmcl
	  lispworks
	  sbcl)
    (second (function-lambda-expression function))
    #+openmcl        (declare (ignore function))
    #+openmcl        (ccl:arglist function-name)
    #+lispworks      (lw:function-lambda-list function)
    #+sbcl (sb-introspect:function-lambda-list function)))

(defun function-definition (function-name)
  "Construct definition for FUNCTION-NAME."
  (make-definition (cond ((macro-function-p function-name)
			  :macro)
			 ((generic-function-p function-name)
			  :generic-function)
			 (t
			  :function))
		   :lambda-list (lambda-list function-name)
		   :documentation (documentation function-name
						 'function)))

(defun class-precedence-list (class-name)
  "Returns class precedence list for CLASS-NAME."
  (closer-mop:class-precedence-list (find-class class-name)))


(defun class-slot-initargs (class-name)
  "Returns class slot initargs for CLASS-NAME."
  (let ((class (find-class class-name)))
    (mapcan (lambda (slot)
	      (copy-seq (closer-mop:slot-definition-initargs slot)))
            (closer-mop:class-slots class))))

(defun class-definition (class-name)
  "Construct definition for CLASS-NAME."
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
  "Construct definition for TYPE-NAME."
  (make-definition :type :documentation (documentation type-name 'type)))

(defun symbol-definitions (symbol)
  "Returns a list of definitions for SYMBOL."
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

(defun sorted-symbol-list (package)
  "Returns sorted list of symbols in PACKAGE."
  (sort (loop for symbol being the external-symbols in package
	   collect symbol)
	(lambda (symbol-x symbol-y)
	  (string< (symbol-name symbol-x)
		   (symbol-name symbol-y)))))

(defun extract-api (package)
  "Extract external symbol definitions in PACKAGE."
  (values (documentation (find-package package) t)
          (loop for symbol in (sorted-symbol-list package)
             collect (keyword-symbol symbol)
             collect (symbol-definitions symbol))))
