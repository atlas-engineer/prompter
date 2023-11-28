;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "prompter"
  :description "Live-narrowing, fuzzy-matching, extensible prompt framework"
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/prompter"
  :license  "BSD-3 Clause"
  :version "0.1.0"
  :serial t
  :depends-on (alexandria
               calispel
               cl-containers
               closer-mop
               lparallel
               moptilities
               nclasses
               serapeum
               str
               trivial-package-local-nicknames)
  :components ((:file "package")
               (:file "filter-preprocessor")
               (:file "filter")
               (:file "prompter-source")
               (:file "prompter"))
  :in-order-to ((test-op (test-op "prompter/tests"))))

(defsystem "prompter/tests"
  :depends-on ("prompter" "lisp-unit2")
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests")
               (:file "fuzzy")
               (:file "submatches"))
  :perform (test-op (op c)
                    (eval-input
                     "(lisp-unit2:run-tests
                       :package :prompter/tests
                       :run-contexts #'lisp-unit2:with-summary-context)")))
