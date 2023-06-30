;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

;; (defun funcall-with-delay (fun queue delay)
;;   "Call FUN over the last element of QUEUE after DELAY has expired since last pop."
;;   (labels ((drain-queue (queue delay &optional last-input)
;;              (multiple-value-bind (input non-empty?)
;;                  (serapeum:synchronized (queue)
;;                    (lpara.queue:try-pop-queue queue :timeout delay))
;;                (if non-empty?
;;                    (drain-queue queue delay input)
;;                    (setf (drained-p delayed-future) t)
;;                    last-input))))
;;     (funcall fun (drain-queue queue delay))))


(define-class delayed-future ()
  ((fn
     nil
     :reader t
     :writer nil
     :export nil)
   (queue
    (lpara.queue:make-queue)
    :export nil)
   (drained-p
    nil
    :accessor nil
    :export nil)
   (kernel                              ; REVIEW: Not needed?
    lpara:*kernel*
    :reader t
    :writer nil
    :export t)
   (delay
     0.0
     :reader t
     :writer nil       ; REVIEW: Does it make sense to allow delay modification?
     :export t)
   (future
     nil
     :accessor nil
     :export nil)
   ;; (first-wait-p                        ; TODO: Do we need both drained-p and first-wait-p?
   ;;   t
   ;;   :accessor nil
   ;;   :export nil)
   )
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:predicate-name-transformer 'nclasses:always-dashed-predicate-name-transformer)
  (:documentation "A `delayed-future' is like an `lparallel:future' that only starts computing its
result after `delay' has passed since the last `fulfill' call.

This allows to efficiently handle input spam by only processing the last received input every `delay'."))

(defun set-future (delayed-future)
  (flet ((funcall-with-delay (fun queue delay)
           "Call FUN over the last element of QUEUE after DELAY has expired since last pop."
           (labels ((drain-queue (queue delay last-input)
                      (multiple-value-bind (input no-timeout)
                          (lpara.queue:try-pop-queue queue :timeout delay)
                        (if no-timeout
                            (drain-queue queue delay input)
                            ;; An item might be pushed to the queue between timeout when we check.
                            ;;
                            ;; If so, we loop again, because it's practically as
                            ;; if the item had arrived just in time.
                            ;;
                            ;; If not, then we return, marking as `drained-p' so
                            ;; that next time an item is pushed the future is
                            ;; re-created.
                            (let ((loop? nil))
                              (serapeum:synchronized (queue)
                                (if (lpara.queue:queue-empty-p queue)
                                    (setf (slot-value delayed-future 'drained-p) t)
                                    (setf loop? t)))
                              (if loop?
                                  (drain-queue queue delay (lpara.queue:pop-queue queue))
                                  last-input))))))
             ;; We call `lpara.queue:pop-queue' here so that calling `force' on
             ;; an unfulfilled delayed-future blocks.
             (funcall fun (drain-queue queue delay (lpara.queue:pop-queue queue))))))
    (let ((lpara:*kernel* (kernel delayed-future)))
      (setf (slot-value delayed-future 'future)
            (lpara:future (funcall-with-delay (fn delayed-future)
                                              (queue delayed-future)
                                              (delay delayed-future)))))))

(defmethod initialize-instance :after ((delayed-future delayed-future) &key)
  (set-future delayed-future))

(export-always 'fulfill)
(define-generic fulfill ((delayed-future delayed-future) arg)
  "Pass ARG to DELAYED-FUTURE.
- Start computing over ARG after waiting for `delay' since last `fulfill' call.
- If `fulfill' is called while a result is being computed, the current
  computation is discarded and a new one is started over ARG."
  (sera:synchronized ((queue delayed-future))
    (lpara.queue:push-queue arg (queue delayed-future))
    (when (slot-value delayed-future 'drained-p)
      (setf (slot-value delayed-future 'drained-p) nil)
      (set-future delayed-future))))

(export-always 'force)
(define-generic force ((delayed-future delayed-future))
  "Like `lparallel:force'."
  (lpara:force (slot-value delayed-future 'future)))

(export-always 'fulfilledp)
(define-generic fulfilledp ((delayed-future delayed-future))
  "Like `lparallel:fulfilledp'."
  (lpara:fulfilledp (slot-value delayed-future 'future)))
