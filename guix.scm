;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell --container -D -f guix.scm
;;
;; Replace --container by --pure if you still want ASDF to see external
;; libraries in ~/common-lisp, etc.
;;
;;; Code:

(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (guix gexp)
             (guix git-download)
             (guix build-system asdf)
             (gnu packages)
             (gnu packages lisp)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz))

(define-public sbcl-prompter
  (package
    (name "sbcl-prompter")
    (version "0.0.0")
    (source
     (local-file (dirname (current-filename)) #:recursive? #t)
;;;; Or this, in case of contributing to Guix.
     ;; (origin
     ;;   (method git-fetch)
     ;;   (uri (git-reference
     ;;         (url "https://github.com/atlas-engineer/prompter")
     ;;         (commit version)))
     ;;   (file-name (git-file-name "cl-prompter" version))
     ;;   (sha256
     ;;    (base32
     ;;     "SPECIFY-HASH")))
     )
    (build-system asdf-build-system/sbcl)
    ;; We use `cl-*' inputs and not `sbcl-*' ones so that CCL users can also use
    ;; this Guix manifests.
    ;;
    ;; Another reason is to not fail when an input dependency is found in
    ;; ~/common-lisp, which would trigger a rebuild of the SBCL input in the
    ;; store, which is read-only and would thus fail.
    ;;
    ;; The official Guix package should use `sbcl-*' inputs though.
    (native-inputs (list cl-lisp-unit2 sbcl))
    (inputs
     (list
      sbcl-alexandria
      sbcl-calispel
      sbcl-cl-containers
      sbcl-cl-str
      sbcl-closer-mop
      sbcl-lparallel
      sbcl-moptilities
      sbcl-nclasses
      sbcl-serapeum
      sbcl-trivial-package-local-nicknames))
    (synopsis "Live-narrowing, fuzzy-matching, extensible prompt framework")
    (home-page "https://github.com/atlas-engineer/prompter")
    (description
     "This prompter library is heavily inspired by Emacs' minibuffer and
Helm (@url{https://emacs-helm.github.io/helm/}).  It only deals with the
backend side of things, it does not handle any display.

Non-exhaustive list of features:

@itemize
@item Asynchronous suggestion computation.
@item Multiple sources.
@item Multiple return actions.
@item Customizable matching and sorting.
@item Multiple attributes to match and display (also known as 'multiple column
display').
@item Customizable initialization and cleanup functions.
@item Notifications sent when suggestion list is updated.
@item Per-source history.
@item Resumable prompters.
@item Marks actions (event-driven on marks change).
@item Current suggestion actions (event-driven on current suggestion change).
@item Automatically return the prompt when narrowed down to a single suggestion.
@end itemize\n")
    (license license:bsd-3)))

(define-public cl-prompter
  (sbcl-package->cl-source-package sbcl-prompter))

(define-public ecl-prompter
  (sbcl-package->ecl-package sbcl-prompter))

cl-prompter
