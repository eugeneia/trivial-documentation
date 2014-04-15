;;;; Tests for PACKAGE-API (requires PACKAGE-API.TEST-PACKAGE).

(defpackage package-api.test
  (:use :cl :package-api :package-api.test-package)
  (:export :run-tests))

(in-package :package-api.test)

(defun assert-symbol-definitions (symbol definitions)
  (assert (equal (symbol-definitions symbol) definitions)))

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
      :lambda-list (package-api.test-package::foo &rest
                    package-api.test-package::bar)
      :documentation "function")))

  (assert-symbol-definitions
   'generic-function-defn
   '((:kind :generic-function
      :lambda-list (package-api.test-package::foo
                    &optional package-api.test-package::bar)
      :documentation "generic-function")))

  ;;; This works only when called from the CL-USER package.
  (assert-symbol-definitions
   'macro-defn
   '((:kind :macro
      :lambda-list (common-lisp-user::foo &body common-lisp-user::body)
      :documentation "macro")))

  (assert-symbol-definitions
   'setf-defn
   '((:kind :function
      :lambda-list (package-api.test-package::x
                    package-api.test-package::n
                    package-api.test-package::v)
      :documentation "setf")))

  (assert-symbol-definitions
   'structure-defn
   '((:kind :structure :documentation "structure")))

  (assert-symbol-definitions
   'type-defn
   '((:kind :type :documentation "type"))))
