;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :prompter/tests
  (:use :common-lisp :lisp-unit2)
  (:import-from :nclasses #:define-class)
  (:import-from :prompter))

(in-package prompter/tests)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum)
  (trivial-package-local-nicknames:add-package-local-nickname :lpara :lparallel)
  (trivial-package-local-nicknames:add-package-local-nickname :lpara.queue :lparallel.queue))
