;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter/tests)

(define-test test-delayed-future ()
  (let ((lpara:*kernel* (lpara:make-kernel (prompter::cpu-count))))
    (let* ((df (make-instance 'prompter:delayed-future
                              :delay 0.1
                              :fn #'identity))
           (f (lpara:future
                (prompter:force df))))
      (prompter:fulfill df "a")
      (sleep 0.01)
      (prompter:fulfill df "b")
      (sleep 0.02)
      (prompter:fulfill df "c")
      (sleep 0.03)
      (prompter:fulfill df "d")
      (sleep 0.12)
      (let ((f2 (lpara:future (prompter:force df))))
        (prompter:fulfill df "e")
        (sleep 0.1)
        (assert-equal "d" (lpara:force f))
        (assert-equal "e" (lpara:force f2))))))
