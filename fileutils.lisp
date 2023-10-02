;;;-*- Mode: common-lisp; syntax: common-lisp;  base: 10 -*-
;;;
;;;; File and Directory Utilities by Peter Seibel
;;;
;;; ==================================================================================

(cl:provide :fileutils)

(cl:defpackage :fileutils
  (:nicknames :futils)
  (:use :common-lisp)
  (:export #:pathname-as-directory #:list-directory))

(in-package :futils)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

#|
(in-package :gx)
(directory-pathname-p (pathname "C:\\Users\\誠二\\Google ドライブ\\ontologies"))
(directory-pathname-p (pathname "C:\\Users\\誠二\\Google ドライブ\\ontologies\\"))
|#

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
      pathname)))
#|
(in-package :gx)
(pathname-as-directory "C:\\Users\\誠二\\Google ドライブ\\ontologies")
(pathname-as-directory "C:\\Users\\誠二\\Google ドライブ\\ontologies\\")
|#

(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))
#|
(in-package :gx)
(directory-wildcard "C:\\Users\\誠二\\Google ドライブ\\ontologies")
(directory-wildcard "C:\\Users\\誠二\\Google ドライブ\\ontologies\\")
|#

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    #+(or sbcl cmu lispworks)
    (directory wildcard)
    #+openmcl
    (directory wildcard :directories t)
    #+allegro
    (directory wildcard :directories-are-files nil)
    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))
    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))
#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

#|
(in-package :gx)
(list-directory "C:\\Users\\誠二\\Google ドライブ\\ontologies")
(list-directory "C:\\Users\\誠二\\Google ドライブ\\ontologies\\")
|#

(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)
  ;; modified by seiji
  #+allegro
  (if (excl::probe-directory pathname)
      (pathname-as-directory pathname)
    (probe-file pathname))
  #+cmu
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
       (probe-file (pathname-as-file pathname)))
      (ignore-errors
       (let ((directory-form (pathname-as-directory pathname)))
         (when (ext:probe-directory directory-form)
           directory-form))))
  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))
#|
(in-package :gx)
(file-exists-p "C:\\Users\\誠二\\Google ドライブ\\ontologies")
(file-exists-p "C:\\Users\\誠二\\Google ドライブ\\ontologies\\")
(file-exists-p "C:\\Users\\誠二\\Google ドライブ\\ontologies\\rdf\\22-rdf-syntax-ns.ttl")
|#