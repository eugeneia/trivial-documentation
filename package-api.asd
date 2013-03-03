;;;; System definition for package-api.

(defpackage package-api-asd
  (:documentation
   "System definition for {PACKAGE-API}.")
  (:use :cl :asdf))

(in-package :package-api-asd)

(defsystem package-api
  :description
  "Extract API from a package."
  :components ((:file "packages")
	       (:file "extract" :depends-on ("packages")))
  :depends-on ("closer-mop"))
