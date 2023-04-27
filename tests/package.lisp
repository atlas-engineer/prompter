;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :prompter/tests
  (:use :common-lisp :lisp-unit2)
  (:import-from :nclasses #:define-class)
  (:import-from :prompter))

(unless lpara:*kernel* (setf lpara:*kernel*
                             (lpara:make-kernel (or (serapeum:count-cpus) 1))))
