;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(defsystem "prompter"
  :description "Describe prompter here"
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/prompter"
  :license  "BSD-3 Clause"
  :version "0.0.0"
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
               (:file "prompter"))
  :in-order-to ((test-op (test-op "prompter/tests")
                         (test-op "prompter/tests/compilation"))))

(defsystem "prompter/submodules"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-submodule-system)

(defsystem "prompter/tests"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-test-system
  :depends-on ("prompter")
  :targets (:package :prompter/tests)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests")
               (:file "fuzzy")
               (:file "submatches")))

(defsystem "prompter/tests/compilation"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-compilation-test-system
  :depends-on ("prompter")
  :packages (:prompter))
