;;;; Tests for TRIVIAL-DOCUMENTATION (requires
;;;; TRIVIAL-DOCUMENTATION.TEST-PACKAGE).

(defpackage trivial-documentation.test
  (:use :cl :trivial-documentation :trivial-documentation.test-package)
  (:export :run-tests))

(in-package :trivial-documentation.test)

(defun assert-symbol-definitions (symbol definitions)
  (unless (equal (symbol-definitions symbol) definitions)
    (error "Assertion failed:~%(EQUAL~% ~a~% ~a)"
           (symbol-definitions symbol)
           definitions)))

(defun run-tests ()

  (assert-symbol-definitions
   '*variable*
   '((:kind :variable :value 1 :documentation "variable")))

  (assert-symbol-definitions
   '+constant+
   '((:kind :constant :value 2 :documentation "constant")))

  (assert-symbol-definitions
   'class-a-defn
   '((:kind :class
      :precedence-list (class-a-defn standard-object t)
      :initargs (:foo)
      :documentation "class-a")))

  (assert-symbol-definitions
   'class-b-defn
   '((:kind :class
      :precedence-list (class-b-defn standard-object t)
      :initargs (:bar)
      :documentation "class-b")))

  (assert-symbol-definitions
   'class-c-defn
   '((:kind :class
      :precedence-list (class-c-defn class-a-defn class-b-defn
                        standard-object t)
      :initargs (:bar :foo :boom)
      :documentation "class-c")))

  (assert-symbol-definitions
   'function-defn
   '((:kind :function
      :lambda-list (trivial-documentation.test-package::foo &rest
                    trivial-documentation.test-package::bar)
      :documentation "function")))

  (assert-symbol-definitions
   'generic-function-defn
   '((:kind :generic-function
      :lambda-list (trivial-documentation.test-package::foo
                    &optional trivial-documentation.test-package::bar)
      :documentation "generic-function")))

  (assert-symbol-definitions
   'macro-defn
   '((:kind :macro
      :lambda-list (first &body second)
      :documentation "macro")))

  (assert-symbol-definitions
   'macro2-defn
   '((:kind :macro
      :lambda-list (first (&key second) &optional third fourth)
      :documentation "macro2")))

  (assert-symbol-definitions
   'setf-defn
   '((:kind :function
      :lambda-list (trivial-documentation.test-package::x
                    trivial-documentation.test-package::n
                    trivial-documentation.test-package::v)
      :documentation "setf")))

  (assert-symbol-definitions
   'structure-defn
   '((:kind :structure :documentation "structure")))

  (assert-symbol-definitions
   'type-defn
   '((:kind :type :documentation "type"))))
