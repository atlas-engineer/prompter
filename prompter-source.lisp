;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

;; TODO: Use methods instead of slots?  Probably no, because we must be able to
;; handle anonymous sources / prompters.
;; TODO: Memoize `suggestion' computation?
;; TODO: User classes?  Probably useful mostly for `source' since
;; they may be defined globally.  Conversely, `prompter' is mostly used
;; locally.

;; TODO: Performance: plists are faster, especially when it comes to modifying
;; existing attributes.

(deftype function-symbol ()
  `(and symbol (satisfies fboundp)))

(defun object-public-slots (object-specifier)
  "Return the list of exported slots."
  (delete-if
   (complement #'exported-p)
   (slot-names object-specifier)))

(define-class source ()
  ((name
    (error "Source must have a name")
    :documentation "Name which can be used to differentiate sources from one
another.")

   (prompter
    nil
    :type (or null prompter)
    :export nil                         ; TODO: Export?
    :documentation "The parent prompter.")

   (kernel                              ; TODO: Can we somehow avoid this?
    nil
    :type (or null lpara:kernel)
    :writer t
    :reader nil
    :export nil
    :documentation "Lparallel fallback kernel in case there is no `prompter' for the source.")

   (constructor
    nil
    :type (or list function)
    :documentation "Function or list to set `initial-suggestions'.
If a function, it's called asynchronously with the source as argument.
The returned value is assigned to `initial-suggestions'.

If a list, it's assigned synchronously to `initial-suggestions'.  The list is
guaranteed to never be modified.")

   (destructor
    nil
    :type (or null function)
    :documentation "Function called with the source as parameter to clean it up.
It's called when `destroy' is called over `prompter'.")

   (initial-suggestions
    '()
    :reader initial-suggestions
    :documentation "Suggestions used on initialization, before any user input is
processed.
On initialization this list is transformed to a list of `suggestion's with
`suggestion-maker'.
This list is never modified after initialization.")

   (initial-suggestions-channel
    nil
    :type (or null lpara:channel)
    :export nil
    :initarg nil
    :documentation "Synchronize `initial-suggestions' access.")

   (suggestions
    '()
    :reader suggestions
    :export t
    :documentation "The current list of suggestions.
It's updated asynchronously every time the prompter input is changed.
The slot is readable even when the computation hasn't finished.
See `ready-p' to know when the list is final.
See `update-notifier' to know when it has been updated, to avoid polling the
list.")

   (marks
    '()
    :type list
    :reader t
    :writer nil
    :export t
    :documentation "The list of `suggestion' values which have been marked by
the user.

Marking is only allowed when `enable-marks-p' is non-nil.  When suggestions
are marked, subsequent `actions-on-return' run over all marked suggestions.

We store the values instead of the `suggestion' because `suggestion' objects are
reinstantiated between each input processing.")

   (actions-on-marks
    #'identity
    :type (or function function-symbol (cons (or function function-symbol) *))
    :documentation "The first function of this list is called automatically when
the marks change.
It does not interrupt or return the prompter.
For convenience, it may be initialized with a single function, in which case it
will be automatically turned into a list.")

   (active-attributes-keys
    '()
    :export t
    :accessor nil
    :documentation "Keys of the `suggestion' attributes to display and process
when filtering.  An empty list means all attributes are displayed.")

   (hide-attribute-header-p
    :never                            ; TODO: Remove `-p' as it's not a boolean.
    :type (member :always :never :single)
    :documentation "Let know the caller whether the attribute names are meant to
be displayed or not.
- When `:always', the column attribute header should be hidden.
- When `:never', the column attribute header should be shown.
- When `:single', it's hidden if there is only one active attribute.")

   (hide-suggestion-count-p
    nil
    :type boolean
    :documentation "Whether the `suggestion' count is displayed.")

   (suggestion-maker
    #'make-suggestion
    :type (or function function-symbol)
    :documentation "Function that wraps an arbitrary object into a source
`suggestion'.
This is useful to set the `suggestion' slots such as `attributes' and
`match-data' depending on the source and the input.

Called on
- arbitrary object
- (optional) source
- (optional) current input.")

   (filter
    #'fuzzy-match
    :type (or null function function-symbol)
    :documentation "Takes a `suggestion', the `source' and the `input' and
return a new `suggestion', or nil if the `suggestion' is discarded.")

   (filter-preprocessor
    #'delete-inexact-matches
    :type (or null function function-symbol)
    :documentation "Function called when input is modified, before `filter'ing the
`suggestion's.
It is passed the following arguments:
- a copy of `initial-suggestions';
- the source;
- the input.")

   (filter-postprocessor
    nil
    :type (or null function function-symbol)
    :documentation "Function called when input is modified, after `filter'ing the
`suggestion's.
It is passed the following arguments:
- the filtered suggestions;
- the source;
- the input.")

   (current-input-downcase-p
    nil
    :type boolean
    :export nil
    :documentation "Whether input is downcased.
This is useful for filters to avoid recomputing it every time.")

   (last-input-downcase-p
    nil
    :type boolean
    :export nil
    :documentation "Whether previous input was downcased.  This is useful to
know if there is a case difference since last time and to know if we have to
recompute the match-data for instance.")

   (sort-predicate
    #'score>
    :type (or null function)
    :documentation "A predicate used to sort the `suggestion's once filtered.
The predicate works the same as the `sort' predicate.")

   (actions-on-return
    #'identity
    :type (or function function-symbol (cons (or function function-symbol) *))
    :accessor nil
    :export nil
    :documentation "List of funcallables that can be run on `suggestion's of
this source.  This is the low-level implementation, see the `actions-on-return'
function for the public interface.
For convenience, it may be initialized with a single function or symbol, in
which case it will be automatically turned into a list.")

   (notification-delay
    0.1
    :type alex:non-negative-real
    :documentation "Time in seconds after which to notify `update-notifier' if
`suggestions' was modified.")

   (ready-p
    nil
    :type boolean
    :export t
    :reader t
    :initarg nil
    :documentation "Whether the source is done computing its suggestions.  See
also `next-ready-p' and `all-ready-p' to wait until ready.")

   (enable-marks-p
    nil
    :type boolean
    :documentation "Whether multiple `suggestion's can be marked.")

   (resumer
    nil
    :type (or null function)
    :documentation "Function meant to be called with the source as argument when
the prompter is resumed.
See `resume-sources'.")

   (actions-on-current-suggestion
    #'identity
    :type (or function function-symbol (cons (or function function-symbol) *))
    :documentation "The first function of this list is called automatically on
the current-suggestion when it's changed.
It does not interrupt or return the prompter.
For convenience, it may be initialized with a single function, in which case it
will be automatically turned into a list.")

   (actions-on-current-suggestion-enabled-p
    nil
    :type boolean
    :documentation "Whether the first of `actions-on-current-suggestion' is
automatically executed.  Also see `actions-on-current-suggestion-delay'.")

   (actions-on-current-suggestion-delay
    0.0
    :type alex:non-negative-real
    :documentation "Time in seconds after which `actions-on-current-suggestion'
run.  Also see `actions-on-current-suggestion-enabled-p'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:predicate-name-transformer 'nclasses:always-dashed-predicate-name-transformer)
  (:documentation "A prompter source instance is meant to be used by a
`prompter' object.  See its `sources' slot.  A source is a consistent collection
of suggestions, filters and actions.

When a `prompter' `input' is set, the `update' function is called over all
sources.  This function pipelines `initial-suggestions' through
`filter-preprocessor', `filter', and finally `filter-postprocessor'.  If any of
these functions is nil, it's equivalent to passing the suggestions unchanged.

`filter-preprocessor' and `filter-postprocessor' are passed the whole list of
suggestions; they only set the `suggestion's once they are done.  Conversely,
`filter' is passed one `suggestion' at a time and it updates `suggestion's on each
call."))

(defmethod kernel ((source source))
  (if (prompter source)
      (kernel (prompter source))
      (or (slot-value source 'kernel)
          (setf (kernel source)
                (lpara:make-kernel (cpu-count))))))

(defmacro with-kernel (holder &body body)
  "Helper to to bind local kernel."
  `(alex:when-let ((lpara:*kernel* (kernel ,holder)))
     ,@body))

(defun default-object-attributes (object)
  `(("Default" ,(princ-to-string object))))

(defmethod (setf marks) (value (source prompter:source))
  (setf (slot-value source 'marks) value)
  (sera:and-let* ((action (alex:ensure-function (first (actions-on-marks source))))
                  (not (eq #'identity action)))
    (with-kernel source
      (lpara:future (funcall action (marks source))))))

(defmethod default-action-on-current-suggestion ((source prompter:source))
  "Return the default action run on the newly selected suggestion.
See `actions-on-current-suggestion'."
  (first (actions-on-current-suggestion source)))

(export-always 'object-attributes)
(defgeneric object-attributes (object source)
  (:method ((object t) (source prompter:source))
    (declare (ignorable source))
    (default-object-attributes object))
  (:method :around ((object t) (source prompter:source))
    (declare (ignorable source))
    ;; TODO: New kernel to not overload the `prompter` threads busy with computing `update'?
    (with-kernel source
      (loop for attribute in (call-next-method)
            for key = (first attribute)
            for value = (second attribute)
            ;; NOTE: Duplicate keys are bad, because searching the alist by key
            ;; will always return the first occurrence, and never the second.
            when (member key keys :test #'string-equal)
              do (warn "Duplicate attribute names found in ~a: ~a.
Attribute names should be unique for prompter to correctly filter those."
                       source key)
            collect key into keys
            ;; FIXME: Having six (string-t for keys and string-function-t for
            ;; values) branches would be more correct, but does that matter enough
            ;; to bother?
            if (functionp value)
              collect (append (list (princ-to-string key)
                                    (lpara:future
                                      (handler-case (funcall (second attribute) object)
                                        (error (c)
                                          (format nil "keyword error: ~a" c)))))
                              (cddr attribute))
            ;; REVIEW: Can keys actually be non-string? Maybe type those?
            else if (and (stringp key) (stringp value))
                   collect attribute
            else collect (append (list (princ-to-string key) (princ-to-string value)) (cddr attribute)))))
  (:method ((object hash-table) (source prompter:source))
    (declare (ignorable source))
    (let ((result))
      (maphash (lambda (key value)
                 (push (list (princ-to-string key)
                             (princ-to-string value))
                       result))
               object)
      (sort result #'string< :key #'first)))
  (:method ((object standard-object) (source prompter:source))
    (declare (ignorable source))
    (or
     (mapcar (lambda (slot)
               (list (string-capitalize (string slot))
                     (princ-to-string (slot-value object slot))))
             (object-public-slots object))
     (call-next-method)))
  (:method ((object structure-object) (source prompter:source))
    (declare (ignorable source))
    (or
     (mapcar (lambda (slot)
               (list (string-capitalize (string slot))
                     (princ-to-string (slot-value object slot))))
             (object-public-slots object))
     (call-next-method)))
  (:method ((object list) (source prompter:source))
    (declare (ignorable source))
    (cond
      ((plist-p object)
       (let ((result '()))
         (alex:doplist (key value object result)
                       (push (list (string-capitalize key) (princ-to-string value))
                             result))
         (nreverse result)))
      ((undotted-alist-p object)
       (mapcar (lambda (pair)
                 (append
                  (list
                   (princ-to-string (first pair))
                   (princ-to-string (second pair)))
                  (cddr pair)))
               object))
      ((alist-p object)
       (mapcar (lambda (pair)
                 (list
                  (princ-to-string (first pair))
                  (princ-to-string (rest pair))))
               object))
      (t (call-next-method))))
  (:documentation "Return an alist of non-dotted lists (ATTRIBUTE-KEY ATTRIBUTE-VALUE ...) for OBJECT.
Attributes are meant to describe the OBJECT in the context of the SOURCE.

The attributes after the first two are for the application specific purposes,
like format strings or code for element display, the calling code can freely use
those to store arbitrary data.

Both returned attribute-keys and attribute-values are strings (if not, they are
automatically converted to `princ-to-string'). If the attribute value is a
function, it's not converted, but rather used for asynchronous attribute
computation.

For structure and class instances, the alist is made of the exported slots: the
keys are the sentence-cased slot names and the values the slot values passed to
`princ-to-string'.

It's used in `make-suggestion' which can be used as a `suggestion-maker' for `source's.

It's useful to separate concerns and compose between different object attributes
and different sources (for instance, the same `object-attributes' method can be
inherited or used across different sources)."))

(define-class suggestion ()
  ((value
    nil ; TODO: Rename `data' as with the GHT?  Maybe confusing since we have `match-data'.
    :type t)
   (attributes
    '()
    :documentation "A non-dotted alist of attributes to structure the filtering.
Both the key and the value are strings or functions.
If a function, it is run asynchronously and must return a string.")
   (match-data
    nil
    :type t
    :documentation "Arbitrary data that can be used by the `filter' function and
its preprocessors.  It's the responsibility of the filter to ensure the
match-data is ready for its own use.")
   (score
    0.0
    :documentation "A score the can be set by the `filter' function and used by
the `sort-predicate'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:predicate-name-transformer 'nclasses:always-dashed-predicate-name-transformer)
  (:documentation "Suggestions are processed and listed in `source'.
It wraps arbitrary object stored in the `value' slot.
The other slots are optional.

Suggestions are made with the `suggestion-maker' slot from `source'."))

(defun pair-p (object)
  (and (listp object)
       (or (not (listp (rest object)))
           (null (rest (rest object))))))

(defun alist-p (object)
  "Return non-nil if OBJECT is an alist, dotted or undotted."
  (and (listp object)
       (every #'pair-p object)))

(defun undotted-alist-p (object &optional value-type)
  "If VALUE-TYPE is non-nil, check if all values are of the specified type."
  (and (listp object)
       (every #'listp object)
       (every #'listp (mapcar #'rest object))
       (or (not value-type)
           (every (lambda (e) (typep (first e) value-type))
                  (mapcar #'rest object)))))

(defun plist-p (object)
  "Return non-nil if OBJECT is a plist."
  (and (listp object)
       (alex:proper-list-p object)
       (evenp (length object))
       (loop :for x :in object :by #'cddr
             :always (keywordp x))))

(defun object-attributes-p (object)
  (and (listp object)
       (every #'listp object)
       (every #'listp (mapcar #'rest object))
       (every (lambda (e) (or (not (lpara:fulfilledp (first e)))
                              (typep (first e) '(or string function))))
              (mapcar #'rest object))))

(export-always 'attribute-key)
(define-generic attribute-key ((attribute t))
  "Return the attribute key."
  (first attribute))

(export-always 'attribute-value)
(define-generic attribute-value ((attribute t) &key wait-p)
  "Return value of ATTRIBUTE.
If WAIT-P, block until attribute is computed.
Otherwise return a `lparallel:future' it the attribute is not done calculating."
  (if (or wait-p
          (lpara:fulfilledp (second attribute)))
      (lpara:force (second attribute))
      ""))

(export-always 'attributes-keys)
(define-generic attributes-keys ((attributes t))
  "Return the list of ATTRIBUTES keys."
  (mapcar #'attribute-key attributes))

(export-always 'attributes-values)
(define-generic attributes-values ((attributes t) &key wait-p)
  "Return the list of ATTRIBUTES values.
See `attribute-value'."
  (mapcar (lambda (a) (attribute-value a :wait-p wait-p)) attributes))

(export-always 'attribute-options)
(define-generic attribute-options ((attribute t))
  "Return the options of ATTRIBUTE, if any."
  (cddr attribute))

(export-always 'attributes-options)
(define-generic attributes-options ((attributes t))
  "Return the options of ATTRIBUTE, if any."
  (mapcar #'attribute-options attributes))

(defun ensure-string (object)
  "Return \"\" if OBJECT is not a string."
  (if (stringp object)
      object
      ""))

(defun format-attributes (attributes)
  "Performance bottleneck: This function is called as many times as they are
suggestions."
  (sera:string-join (mapcar #'ensure-string (attributes-values attributes)) " "))

(defmethod initialize-instance :after ((suggestion suggestion) &key)
  "Check validity."
  (unless (object-attributes-p (attributes suggestion))
    (warn "Attributes of ~s should be a non-dotted alist instead of ~s" (value suggestion) (attributes suggestion))
    (setf (attributes suggestion) (default-object-attributes (value suggestion)))))

(defun ensure-match-data-string (suggestion source)
  "Return SUGGESTION's `match-data' as a string.
If unset, set it to the return value of `format-attributes'."
  (flet ((maybe-downcase (s)
           (if (current-input-downcase-p source)
               (string-downcase s)
               s)))
    (setf (match-data suggestion)
          (if (and (match-data suggestion)
                   (typep (match-data suggestion) 'string))
              (if (not (eq (last-input-downcase-p source)
                           (current-input-downcase-p source)))
                  (maybe-downcase (match-data suggestion))
                  (match-data suggestion))
              (let ((result (format-attributes (attributes suggestion))))
                (maybe-downcase result)))))
  (match-data suggestion))

(export-always 'make-suggestion)
(defgeneric make-suggestion (value &optional source input)
  (:method ((value t) &optional source input)
    (declare (ignore input))
    (make-instance 'suggestion
                   :value value
                   :attributes (object-attributes value source)))
  (:documentation "Return a `suggestion' wrapping around VALUE.
Attributes are set with `object-attributes'."))

(defgeneric default-action-on-return (source)
  (:method ((source source))
    (first (slot-value source 'actions-on-return)))
  (:documentation "Return the default action run when returning from the prompt.
See `actions-on-return'."))

(define-class yes-no-source (source)
  ((name "Confirm")
   (yes "yes")
   (no "no")
   (constructor (list t nil))
   (hide-attribute-header-p :always))
  (:export-class-name-p t)
  (:predicate-name-transformer 'nclasses:always-dashed-predicate-name-transformer)
  (:documentation "Prompt source for yes-no questions."))

(defmethod object-attributes ((object symbol) (source yes-no-source))
  `(("Answer" ,(if object (yes source) (no source)))))

(defun make-input-suggestion (suggestions source input)
  (declare (ignore suggestions))
  (list (funcall (suggestion-maker source) input
                 source input)))

(define-class raw-source (source)
  ((name "Input")
   (filter-preprocessor 'make-input-suggestion)
   (filter nil)
   (hide-attribute-header-p :always)
   (hide-suggestion-count-p t)
   (enable-marks-p nil))
  (:export-class-name-p t)
  (:predicate-name-transformer 'nclasses:always-dashed-predicate-name-transformer)
  (:documentation "Prompt source for raw user input.
Its only `suggestion' is the user input, thus it has no constructor.
If you are looking for a source that just returns its plain suggestions, use `source'."))

(defmethod initialize-instance :after ((raw-source raw-source) &key)
  "Report error when RAW-SOURCE is provided bad initial arguments."
  (when (constructor raw-source)
    (error "Raw source should have no constructor: ~a" (constructor raw-source))))

(defun make-word-suggestions (suggestions source input)
  (declare (ignore suggestions))
  (mapcar (lambda (word)
            (funcall (suggestion-maker source) word
                     source input))
          (sera:words input)))

(define-class word-source (source)
  ((name "Input words")
   (filter-preprocessor 'make-word-suggestions)
   (filter nil)
   (hide-attribute-header-p :always)
   (enable-marks-p t))
  (:export-class-name-p t)
  (:predicate-name-transformer 'nclasses:always-dashed-predicate-name-transformer)
  (:documentation "Prompt source for user input words."))

(export-always 'ensure-suggestions-list)
(define-generic ensure-suggestions-list ((source source) elements)
  "Return ELEMENTS as a list of suggestions for use in SOURCE."
  (with-kernel source
    (lpara:pmapcar
     (lambda (suggestion-value)
       (if (suggestion-p suggestion-value)
           suggestion-value
           (funcall (suggestion-maker source)
                    suggestion-value
                    source)))
     (uiop:ensure-list elements))))

(defmethod initialize-instance :after ((source source) &key)
  "See the `constructor' documentation of `source'."
  (etypecase (constructor source)
    (list
     (setf (slot-value source 'initial-suggestions) (ensure-suggestions-list
                                                     source
                                                     (constructor source))
           ;; `suggestions' are in `update'.  So if `update' hasn't run yet, the
           ;; prompter would initially have no suggestion.  It can be a problem
           ;; if the preprocessor is slow, which is why we initialize
           ;; `suggestions' here.
           (slot-value source 'suggestions) (initial-suggestions source)))
    (t ;; Async construction:
     (with-kernel source
       (setf (initial-suggestions-channel source) (lpara:make-channel))
       (lpara:submit-task (initial-suggestions-channel source)
                          (lambda ()
                            (setf (slot-value source 'initial-suggestions)
                                  (ensure-suggestions-list source
                                                           (funcall (constructor source) source)))
                            (setf (slot-value source 'suggestions) (initial-suggestions source)))))))
  (setf (actions-on-current-suggestion source)
        (uiop:ensure-list (or (actions-on-current-suggestion source)
                              #'identity)))
  (setf (actions-on-marks source)
        (uiop:ensure-list (or (actions-on-marks source)
                              #'identity)))
  (setf (slot-value source 'actions-on-return)
        (uiop:ensure-list (or (slot-value source 'actions-on-return)
                              #'identity)))
  source)

(export-always 'attributes-keys-non-default)
(define-generic attributes-keys-non-default ((source source))
  "Return SOURCE attributes except the default one."
  (rest (attributes-keys source)))

(export-always 'attributes-keys-default)
(define-generic attributes-keys-default ((source source))
  "Return SOURCE default attribute as a non-dotted pair."
  (first (attributes-keys source)))

(export-always 'attributes-default)
(define-generic attributes-default ((suggestion suggestion))
  "Return SUGGESTION default attribute value."
  (attribute-value (first (attributes suggestion))))

(export-always 'attributes-non-default)
(define-generic attributes-non-default ((suggestion suggestion))
  "Return SUGGESTION non-default attributes."
  (rest (attributes suggestion)))

(defmethod attributes-keys ((source source))
  (attributes-keys
   (alex:if-let ((sugg (first (suggestions source)))) ; TODO: Instead, ensure that SUGGESTIONS always has an element?
     (attributes sugg)
     (default-object-attributes ""))))

(define-generic active-attributes-keys ((source source))
  "Return active attributes keys.
If the `active-attributes' slot is NIL, return all attributes keys."
  (or (slot-value source 'active-attributes-keys)
      (attributes-keys source)))

(defmethod (setf active-attributes-keys) (value (source source))
  "Set active attributes to the intersection of VALUE and SOURCE attributes."
  (flet ((remove-from-seq (seq &rest items)
           (reduce (lambda (seq item) (remove item seq :test #'string=))
                   (set-difference seq items :test #'string=)
                   :initial-value seq)))
    (setf (slot-value source 'active-attributes-keys)
          (cons (attributes-keys-default source)
                (apply #'remove-from-seq (attributes-keys-non-default source) value)))))

(export-always 'active-attributes)
(define-generic active-attributes ((suggestion suggestion)
                                   &key (source (error "Source required"))
                                   &allow-other-keys)
  "Return the active attributes of SUGGESTION.
Active attributes are attributes whose keys are listed in the
`active-attributes-keys' slot of SOURCE."
  (let ((inactive-keys (set-difference (attributes-keys (attributes suggestion))
                                       (active-attributes-keys source)
                                       :test #'string=)))
    (remove-if
     (lambda (attr)
       (find (attribute-key attr) inactive-keys :test #'string=))
     (attributes suggestion))))

(export-always 'marked-p)
(defun marked-p (source value)
  "Return non-nil if VALUE is marked in SOURCE.
Comparison is done with `equalp'."
  (find value (prompter:marks source) :test #'equalp))

(defun maybe-funcall (fn &rest args)
  "Funcall FN over args.
If FN is nil, return ARGS as multiple values."
  (if fn
      (apply fn args)
      (apply #'values args)))

(defun insert-item-at (item pred sequence) ; TODO: Arg order? Name?
  "Insert ITEM in SEQUENCE after the last item FOO for which (PRED ITEM FOO) is
non-nil."
  (if sequence
      (let ((item-pos
              (or (position-if (lambda (e) (funcall pred item e)) sequence)
                  (length sequence))))
        (nconc (subseq sequence 0 item-pos)
               (list item)
               (subseq sequence item-pos)))
      (list item)))

(defun copy-object (object &rest slot-overrides)
  (let ((class-sym (class-name (class-of object))))
    (apply #'make-instance class-sym
           (append
            slot-overrides
            (alexandria:mappend
             (lambda (slot)
               (list (intern (symbol-name slot) "KEYWORD")
                     (slot-value object slot)))
             (slot-names class-sym))))))

(define-generic destroy ((source source))
  "Clean up the source.
SOURCE should not be used in a prompter once this has been run, but its
`suggestions' can still be accessed."
  (maybe-funcall (destructor source) source)
  (alex:when-let ((lpara:*kernel* (slot-value source 'kernel)))
    (lpara:kill-tasks :default)
    (lpara:end-kernel)
    (setf (kernel source) nil)))

(defun update (source input)            ; TODO: Store `input' in the source?
  "Update SOURCE to narrow down the list of `suggestion's according to INPUT.
If a previous `suggestion' computation was not finished, it is forcefully
terminated.

- First the `filter-preprocessor' is run over a copy of `initial-suggestions'.
- The resulting suggestions are passed one by one to `filter'.
  When filter returns non-nil, the result is added to `suggestions' and
  `update-notifier' is notified, if `notification-delay' has been exceeded or if
  the last `suggestion' has been processed.
- Last the `filter-postprocessor' is run the SOURCE and the INPUT.
  Its return value is assigned to the list of suggestions.
- Finally, `ready-p' is set to T.

The reason we filter in 3 stages is to allow both for asynchronous and
synchronous filtering.  The benefit of asynchronous filtering is that it sends
feedback to the user while the list of suggestions is being computed."
  (labels ((run-hook (source)
             (lpara:future (funcall (update-hook (prompter source)) source)))
           (wait-for-initial-suggestions ()
             (unless (or (initial-suggestions source)
                         (not (constructor source)))
               (setf (slot-value source 'initial-suggestions)
                     (lpara:receive-result (initial-suggestions-channel source)))))
           (preprocess (initial-suggestions-copy)
             (if (filter-preprocessor source)
                 (ensure-suggestions-list
                  source
                  (funcall (filter-preprocessor source)
                           initial-suggestions-copy source input))
                 initial-suggestions-copy))
           (process! (preprocessed-suggestions)
             (let ((last-notification-time (get-internal-real-time)))
               ;; `last-notification-time' is needed to check if
               ;; `notification-delay' was exceeded, after which `update-notifier'
               ;; is notified.
               (setf (slot-value source 'suggestions) '())
               (if (or (str:empty? input)
                       (not (filter source)))
                   (setf (slot-value source 'suggestions) preprocessed-suggestions)
                   (dolist (suggestion preprocessed-suggestions)
                     (alex:when-let ((suggestion (funcall (filter source) suggestion source input)))
                       (setf (slot-value source 'suggestions)
                             (insert-item-at suggestion (sort-predicate source)
                                             (suggestions source)))
                       (let* ((now (get-internal-real-time))
                              (duration (/ (- now last-notification-time)
                                           internal-time-units-per-second)))
                         (when (or (> duration (notification-delay source))
                                   (= (length (slot-value source 'suggestions))
                                      (length preprocessed-suggestions)))
                           (run-hook source)
                           (setf last-notification-time now))))))))
           (postprocess! ()
             (when (filter-postprocessor source)
               (setf (slot-value source 'suggestions)
                     (ensure-suggestions-list
                      source
                      (maybe-funcall (filter-postprocessor source)
                                     (slot-value source 'suggestions)
                                     source
                                     input))))))
    (unwind-protect
         (progn
           (setf (slot-value source 'ready-p) nil)
           (wait-for-initial-suggestions)
           (setf (last-input-downcase-p source) (current-input-downcase-p source))
           (setf (current-input-downcase-p source) (str:downcasep input))
           (run-hook source)
           (process!
            (preprocess
             ;; We copy the list of initial-suggestions so that the
             ;; preprocessor cannot modify them.
             (mapcar #'copy-object (initial-suggestions source))))
           (postprocess!))
      (setf (slot-value source 'ready-p) t)
      ;; Also run hook once SOURCE is ready, so that handlers can reliably check
      ;; for `ready-p'.
      (run-hook source)))
  source)
