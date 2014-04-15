;;;; Package definitions for PACKAGE-API.

(defpackage :package-api
  (:documentation
   "Extract API from a package.")
  (:use :cl)
  (:export :symbol-definitions
           :extract-api))
