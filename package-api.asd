;;;; System definition for PACKAGE-API.

(defsystem package-api
  :description
  "Extract documentation and definitions for symbols and packages."
  :components ((:file "packages")
	       (:file "extract" :depends-on ("packages")))
  :depends-on ("closer-mop"))
