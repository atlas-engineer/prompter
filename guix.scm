;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm
;;
;; To start the REPL:
;;
;;   guix shell -f guix.scm sbcl -- sbcl
;;
;;; Code:

(use-modules (guix packages)
             (guix gexp)
             (guix build-system asdf)
             (gnu packages)
             (gnu packages lisp)
             (gnu packages lisp-xyz))

(define-public sbcl-prompter-dev
  (package
    (inherit sbcl-prompter)
    (source
     (local-file (dirname (current-filename))
                 #:recursive? #t
                 #:select? (lambda (file stat) (not (string=? (basename file) "nasdf")))))))

(define-public cl-prompter-dev
  (sbcl-package->cl-source-package sbcl-prompter-dev))

(define-public ecl-prompter-dev
  (sbcl-package->ecl-package sbcl-prompter-dev))

cl-prompter-dev
