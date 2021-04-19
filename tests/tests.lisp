;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(prove:plan nil)

(defun source1-suggestions (prompter)
  (mapcar #'prompter:value (prompter:suggestions
                            (first (prompter:sources prompter)))))

(defun all-source-suggestions (prompter)
  (mapcar #'prompter:value (alex:mappend #'prompter:suggestions
                                         (prompter:sources prompter))))

(prove:subtest "Prompter init"
  (let ((prompter (prompter:make
                   :sources (make-instance 'prompter:source
                             :name "Test source"
                             :constructor '("foo" "bar")))))
    (prove:ok (find "foo" (prompter:suggestions
                           (first (prompter:sources prompter)))
                    :test #'string=
                    :key #'prompter:value)
              "Found suggestion in dummy prompter")))

(prove:subtest "Prompter matching"
  (let ((prompter (prompter:make
                   :sources (list (make-instance 'prompter:source
                                   :name "Test source"
                                   :constructor '("foo" "bar"))))))
    (setf (prompter:input prompter) "foo")
    (when (prompter:all-ready-p prompter)
      (prove:is (source1-suggestions prompter)
                '("foo")))
    (setf (prompter:input prompter) "bar")
    (when (prompter:all-ready-p prompter)
      (prove:is (source1-suggestions prompter)
                '("bar")))
    (setf (prompter:input prompter) "")
    (when (prompter:all-ready-p prompter)
      (prove:is (source1-suggestions prompter)
                '("foo" "bar")))))

(class-star:define-class url ()
  ((uri "")
   (title ""))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defmethod prompter:object-attributes ((url url))
  `(:uri ,(uri url)
    :title ,(title url)))

(prove:subtest "Multi-attribute matching"
  (let* ((url1 (make-instance 'url :uri "http://example.org" :title "Example"))
         (url2 (make-instance 'url :uri "http://nyxt.atlas.engineer" :title "Nyxt homepage"))
         (prompter (prompter:make
                    :sources (list (make-instance 'prompter:source
                                    :name "Test source"
                                    :constructor (list url1 url2))))))
    (setf (prompter:input prompter) "nyxt")
    (when (prompter:all-ready-p prompter)
      (let ((filtered-suggestions (prompter:suggestions
                                   (first (prompter:sources prompter)))))
        (prove:is (mapcar #'prompter:value filtered-suggestions)
                  (list url2))))))

(defvar *prompter-suggestion-update-interval* 1.5)

(defun slow-identity-match (input suggestion)
  (declare (ignore input))
  (sleep *prompter-suggestion-update-interval*)
  suggestion)

(prove:subtest "Asynchronous suggestion computation"
  (let ((prompter (prompter:make
                   :sources (list (make-instance 'prompter:source
                                   :name "Test source"
                                   :constructor '("foo" "bar")
                                   :filter #'slow-identity-match)))))
    (setf (prompter:input prompter) "foo")
    (when (prompter:all-ready-p prompter)
      (let ((filtered-suggestions (prompter:suggestions
                                   (first (prompter:sources prompter)))))
        (prove:is (mapcar #'prompter:value filtered-suggestions)
                  '("foo"))))))

(prove:subtest "Asynchronous suggestion notifications"
  (let* ((suggestion-values '("foobar" "foobaz"))
         (source (make-instance 'prompter:source
                  :name "Test source"
                  :constructor suggestion-values
                  :filter #'slow-identity-match))
         (prompter (prompter:make
                    :sources (list source))))
    (setf (prompter:input prompter) "foo")
    (sera:nlet query-suggestions ((computed-count 1))
      (calispel:fair-alt
        ((calispel:? (prompter::ready-channel source))
         (prove:is (length (prompter:suggestions source))
                   (length suggestion-values)))
        ((calispel:? (prompter:update-notifier source))
         (prove:is (length (prompter:suggestions source))
                   computed-count)
         (query-suggestions (1+ computed-count)))))))

(prove:subtest "Asynchronous suggestion interrupt"
  (let* ((suggestion-values '("foobar" "foobaz"))
         (source (make-instance 'prompter:source
                  :name "Test source"
                  :constructor suggestion-values
                  :filter #'slow-identity-match))
         (prompter (prompter:make
                    :sources (list source))))
    (let ((before-input (get-internal-real-time)))
      (setf (prompter:input prompter) "foo")
      (setf (prompter:input prompter) "bar")
      (setf (prompter:input prompter) "baz")
      (prove:is (/ (- (get-internal-real-time) before-input)
                   internal-time-units-per-second)
                0.01
                :test #'<
                "Consecutive inputs happened fast enough"))))

(prove:subtest "Yes-No prompt"
  (let* ((source (make-instance 'prompter:yes-no-source
                                :constructor '("no" "yes")))
         (prompter (prompter:make
                    :sources (list source))))
    (prove:is
     (mapcar #'prompter:value (prompter:suggestions
                               (first (prompter:sources prompter))))
     '("no" "yes"))
    (setf (prompter:input prompter) "y")
    (when (prompter:all-ready-p prompter)
      (let ((filtered-suggestions (prompter:suggestions
                                   (first (prompter:sources prompter)))))
        (prove:is (mapcar #'prompter:value filtered-suggestions)
                  '("yes" "no"))))))

(prove:subtest "Return result"
  (let ((prompter (prompter:make
                   :sources (list (make-instance 'prompter:source
                                   :name "Test source"
                                   :constructor '("foo" "bar"))))))
    (setf (prompter:input prompter) "bar")
    (when (prompter:all-ready-p prompter)
      (prompter:return-selection prompter)
      (prove:is (calispel:? (prompter:result-channel prompter))
                '("bar")))))

(prove:subtest "Multi sources"
  (let ((prompter (prompter:make
                   :sources (list (make-instance 'prompter:source
                                   :name "Test source 1"
                                   :constructor '("foo" "bar"))
                                  (make-instance 'prompter:source
                                   :name "Test source 2"
                                   :constructor '("100 foo" "200"))))))
    (setf (prompter:input prompter) "foo")
    (when (prompter:all-ready-p prompter)
      (prove:is (all-source-suggestions prompter)
                '("foo" "100 foo")))
    (setf (prompter:input prompter) "200")
    (let ((ready-source1 (prompter:next-ready-p prompter))
          (ready-source2 (prompter:next-ready-p prompter)))
      (prove:ok (find ready-source1 (prompter:sources prompter))
                "Found first ready source")
      (prove:ok (find ready-source2 (prompter:sources prompter))
                "Found second ready source")
      (prove:isnt ready-source1
                  ready-source2
                  "Ready sources are not the same")
      (prove:is (all-source-suggestions prompter)
                '("foo" "bar" "200")))))

(prove:subtest "Raw source"
  (let ((prompter (prompter:make
                   :sources (list (make-instance 'prompter:raw-source)))))
    (setf (prompter:input prompter) "foo")
    (when (prompter:all-ready-p prompter)
      (prove:is (all-source-suggestions prompter)
                '("foo"))))
  (let ((prompter (prompter:make
                   :input "foo"
                   :sources (list (make-instance 'prompter:raw-source)))))
    (when (prompter:all-ready-p prompter)
      (prove:is (all-source-suggestions prompter)
                '("foo")))))

(prove:finalize)
