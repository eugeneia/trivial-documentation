;;;; Package definitions for package-api.

(defpackage :package-api
  (:description
   "Extract API from a package.")
  (:use :cl)
  (:export :extract-api))
