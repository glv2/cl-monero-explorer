;;;; This file is part of monero-explorer
;;;; Copyright 2019-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-explorer-mcclim
  (:use :cl)
  (:import-from :monero-explorer-common
                #:lookup-block
                #:lookup-transaction)
  (:import-from :monero-rpc
                #:*rpc-host*
                #:*rpc-password*
                #:*rpc-port*
                #:*rpc-user*)
  (:export #:gui))

(in-package :monero-explorer-mcclim)


(clim:define-application-frame explorer-frame ()
  ((query-result :initform '(:empty) :accessor query-result)
   (password :initform (make-array 0
                                   :element-type 'character
                                   :adjustable t
                                   :fill-pointer t)
             :accessor password))
  (:menu-bar nil)
  (:panes
   (host-label :label :label "Host" :width 50 :max-width 50)
   (host :text-field :value "127.0.0.1")
   (port-label :label :label "Port" :width 50 :max-width 50)
   (port :text-field :value "18081" :width 100 :max-width 100)
   (user-label :label :label "User" :width 50 :max-width 50)
   (user :text-field :value "" :width 150 :max-width 150)
   (password-label :label :label "Password" :width 50 :max-width 50)
   (password :text-field
             :value ""
             :width 150
             :max-width 150
             :value-changed-callback #'read-password)
   (query :text-field
          :value "Enter hash or height"
          :activate-callback #'lookup)
   (copy :push-button
         :max-width 50
         :label "Copy"
         :activate-callback #'copy-query)
   (paste :push-button
          :max-width 50
          :label "Paste"
          :activate-callback #'paste-query)
   (lookup :push-button
           :max-width 50
           :label "Lookup"
           :activate-callback #'lookup)
   (result :application
           :scroll-bars :both
           :display-function #'display-result))
  (:layouts
   (default (clim:vertically (:height 800)
              (clim:spacing (:thickness 5)
                (clim:horizontally (:spacing 20)
                  (clim:horizontally (:spacing 10)
                    host-label host)
                  (clim:horizontally (:spacing 10)
                    port-label port)
                  (clim:horizontally (:spacing 10)
                    user-label user)
                  (clim:horizontally (:spacing 10)
                    password-label password)))
              (clim:spacing (:thickness 5)
                (clim:horizontally (:width 950 :spacing 5)
                  (clim:spacing (:thickness 5) query)
                  copy
                  paste
                  lookup))
              result))))

(defun read-password (pane value)
  (let* ((frame (clim:pane-frame pane))
         (lv (length value))
         (lp (length (password frame))))
    (cond
      ((> lv lp)
       (vector-push-extend (elt value (1- lv)) (password frame)))
      ((< lv lp)
       (setf (fill-pointer (password frame)) lv)))
    (unless (every (lambda (c) (char= #\* c)) value)
      (setf (clim:gadget-value pane) (make-string lv :initial-element #\*))
      (clim:redisplay-frame-panes frame))))

(defun lookup (pane)
  (let* ((frame (clim:pane-frame pane))
         (*rpc-host* (clim:gadget-value (clim:find-pane-named frame 'host)))
         (*rpc-port* (clim:gadget-value (clim:find-pane-named frame 'port)))
         (*rpc-user* (clim:gadget-value (clim:find-pane-named frame 'user)))
         (*rpc-password* (password frame))
         (query (clim:gadget-value (clim:find-pane-named frame 'query))))
    (multiple-value-bind (height length) (parse-integer query :junk-allowed t)
      (let ((id (if (= length (length query)) height query)))
        (multiple-value-bind (info error1)
            (ignore-errors (lookup-block id))
          (if info
              (setf (query-result frame) info)
              (multiple-value-bind (info error2)
                  (ignore-errors (lookup-transaction id))
                (if info
                    (setf (query-result frame) info)
                    (setf (query-result frame) (list :error error1 error2))))))
        (clim:redisplay-frame-pane frame 'result)))))

(defun copy-query (pane)
  (declare (ignore pane))
  (let ((query (clim:find-pane-named clim:*application-frame* 'query)))
    (clim-extensions:publish-selection *standard-output*
                                       :clipboard
                                       (clim:gadget-value query)
                                       'string)))

(defun paste-query (pane)
  (declare (ignore pane))
  (clim:execute-frame-command clim:*application-frame* '(com-paste)))

(defun display-block (pane info)
  (destructuring-bind (block-size hash height major-version minor-version
                       nonce prev-hash reward timestamp
                       miner-tx-hash tx-hashes)
      info
    (let ((*standard-output* pane))
      (terpri)
      (write-string "Block: ")
      (clim:present hash 'string)
      (fresh-line)
      (write-string "Height: ")
      (clim:present (format nil "~d" height) 'string)
      (fresh-line)
      (write-string "Previous block: ")
      (clim:present prev-hash 'string)
      (fresh-line)
      (format t
              "Version: ~d.~d~
               ~%Size: ~d~
               ~%Nonce: ~d~
               ~%Timestamp: ~d~
               ~%Reward: ~d~%"
              major-version minor-version block-size nonce
              timestamp reward)
      (terpri)
      (write-string "Miner transaction: ")
      (format t "~2%~4t")
      (clim:present miner-tx-hash 'string)
      (format t "~2%Transactions:~2%")
      (dolist (tx-hash tx-hashes)
        (format t "~4t")
        (clim:present tx-hash 'string)
        (fresh-line)))))

(defun display-transaction (pane info)
  (destructuring-bind (block-height block-timestamp double-spend-seen
                       in-pool version unlock-time inputs outputs extra)
      info
    (let ((*standard-output* pane))
      (flet ((print-input (input)
               (destructuring-bind (amount key-offsets key-image) input
                 (format nil
                         "~%~4tAmount: ~d~
                          ~%~4tKey offsets: ~{~a~^, ~}~
                          ~%~4tKey image: ~a"
                         amount key-offsets key-image)))
             (print-output (output)
               (destructuring-bind (amount key) output
                 (format nil
                         "~%~4tAmount: ~d~
                          ~%~4tKey: ~a"
                         amount key)))
             (print-extra (extra)
               (destructuring-bind (transaction-public-key
                                    additional-public-keys
                                    payment-id)
                   extra
                 (concatenate 'string
                              (when (plusp (length transaction-public-key))
                                (format nil
                                        "~%Public key: ~a"
                                        transaction-public-key))
                              (when additional-public-keys
                                (format nil
                                        "~%Additional-public-keys:~
                                         ~%~{~4t~a~^~%~}"
                                        additional-public-keys))
                              (when (plusp (length payment-id))
                                (format nil
                                        "~%Payment ID: ~a"
                                        payment-id))))))
        (terpri)
        (write-string "Block height: ")
        (clim:present (format nil "~d" block-height) 'string)
        (fresh-line)
        (format t
                "Block timestamp: ~d~
                 ~%Version: ~d~
                 ~%In pool: ~:[No~;Yes~]~
                 ~%Double spend seen: ~:[No~;Yes~]~
                 ~%Unlock time: ~d~
                 ~a~
                 ~2%Inputs: ~d~
                 ~{~a~%~}~
                 ~%Outputs: ~d~
                 ~{~a~%~}"
                block-timestamp version in-pool
                double-spend-seen unlock-time
                (print-extra extra)
                (length inputs)
                (mapcar #'print-input inputs)
                (length outputs)
                (mapcar #'print-output outputs))))))

(defun display-not-found (pane info)
  (destructuring-bind (error-block error-transaction)
      info
    (format pane
            "~%No information found~
             ~%~%Block request: ~a~
             ~%~%Transaction request: ~a"
            error-block error-transaction)))

(defun display-result (frame stream)
  (let ((info (query-result frame)))
    (case (car info)
      ((:block) (display-block stream (cdr info)))
      ((:transaction) (display-transaction stream (cdr info)))
      ((:error) (display-not-found stream (cdr info))))))

(clim:define-presentation-action copy-string
    (string nil explorer-frame :gesture :select)
    (object)
  (clim-extensions:publish-selection *standard-output* :clipboard object 'string))

(clim:define-presentation-action lookup-string
    (string nil explorer-frame :gesture :select :priority 1)
    (object)
  (let ((query (clim:find-pane-named clim:*application-frame* 'query)))
    (setf (clim:gadget-value query) object)
    (lookup query)))

(define-explorer-frame-command (com-paste :menu nil) ()
  (let ((query (clim:find-pane-named clim:*application-frame* 'query)))
    (setf (clim:gadget-value query)
          (clim-extensions:request-selection *standard-output* :clipboard t))))

(defun gui (&optional new-process)
  (let ((frame (clim:make-application-frame 'explorer-frame
                                            :pretty-name "Monero Explorer")))
    (if new-process
        (clim-sys:make-process (lambda () (clim:run-frame-top-level frame)))
        (clim:run-frame-top-level frame))))
