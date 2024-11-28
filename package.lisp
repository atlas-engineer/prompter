;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :prompter
  (:use :common-lisp)
  (:import-from :nclasses #:define-class)
  (:import-from :serapeum #:export-always))
(in-package prompter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(defmacro define-function (name args &body body)
  "Eval ARGS then define function over the resulting lambda list.
All ARGS are declared as `ignorable'."
  (let ((evaluated-args (eval args)))
    `(defun ,name  ,evaluated-args
       (declare (ignorable ,@(set-difference (mapcar (lambda (arg) (if (listp arg) (first arg) arg))
                                                     evaluated-args)
                                             lambda-list-keywords)))
       ,@body)))

(export-always '*debug-on-error*)
(defvar *debug-on-error* nil
  "When non-nil, the Lisp debugger is invoked when a condition is raised.
Otherwise all errors occuring in threads are demoted to warnings.")
