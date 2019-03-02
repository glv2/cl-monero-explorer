;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-explorer-mcclim
  (:use :cl)
  (:import-from :monero-explorer-common
                #:lookup-block
                #:lookup-transaction)
  (:import-from :monero-tools-rpc
                #:*rpc-host*
                #:*rpc-password*
                #:*rpc-port*
                #:*rpc-user*)
  (:export #:gui))

(in-package :monero-explorer-mcclim)


(clim:define-application-frame explorer-frame ()
  ((query-result :initform '(:empty) :accessor query-result))
  (:menu-bar nil)
  (:panes
   (host-label :label :label "Host" :width 50 :max-width 50)
   (host :text-field :value "127.0.0.1")
   (port-label :label :label "Port" :width 50 :max-width 50)
   (port :text-field :value "18081" :width 100 :max-width 100)
   (user-label :label :label "User" :width 50 :max-width 50)
   (user :text-field :value "" :width 150 :max-width 150)
   (password-label :label :label "Password" :width 50 :max-width 50)
   (password :text-field :value "" :width 150 :max-width 150)
   (query :text-field
          ;;:value "da6ce65f5e0f6e6d46b15fe39613075aba744eb8e30bfee8dd44d9019a941ea1"
          ;;:value "099cc5ce99357a7946b52fd36eafdaef1851b6ede17fce0b05843ae26827692a"
          :value "Enter hash or height"
          :activate-callback #'lookup)
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
                  lookup))
              result))))

(defun lookup (pane)
  (let* ((frame (clim:pane-frame pane))
         (*rpc-host* (clim:gadget-value (clim:find-pane-named frame 'host)))
         (*rpc-port* (clim:gadget-value (clim:find-pane-named frame 'port)))
         (*rpc-user* (clim:gadget-value (clim:find-pane-named frame 'user)))
         (*rpc-password* (clim:gadget-value (clim:find-pane-named frame 'password)))
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

(defun display-block (pane info)
  (destructuring-bind (block-size hash height major-version minor-version
                       nonce prev-hash reward timestamp
                       miner-tx-hash tx-hashes)
      info
    (let* ((text (format nil
                         "~%Block: ~a~
                          ~%Height: ~d~
                          ~%Previous block: ~a~
                          ~%Version: ~d.~d~
                          ~%Size: ~d~
                          ~%Nonce: ~d~
                          ~%Timestamp: ~d~
                          ~%Reward: ~d~
                          ~2%Miner transaction:~2%~4t~a~
                          ~2%Transactions:~2%~{~4t~a~%~}"
                         hash height prev-hash major-version minor-version
                         block-size nonce timestamp reward miner-tx-hash
                         tx-hashes)))
      (clim:draw-text* pane text 2 0))))

(defun display-transaction (pane info)
  (destructuring-bind (block-height block-timestamp double-spend-seen
                       in-pool version unlock-time inputs outputs extra)
      info
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
      (let ((text (format nil
                          "~%Block height: ~d~
                           ~%Block timestamp: ~d~
                           ~%Version: ~d~
                           ~%In pool: ~:[No~;Yes~]~
                           ~%Double spend seen: ~:[No~;Yes~]~
                           ~%Unlock time: ~d~
                           ~a~
                           ~2%Inputs: ~d~
                           ~{~a~%~}~
                           ~%Outputs: ~d~
                           ~{~a~%~}"
                          block-height block-timestamp version in-pool
                          double-spend-seen unlock-time
                          (print-extra extra)
                          (length inputs)
                          (mapcar #'print-input inputs)
                          (length outputs)
                          (mapcar #'print-output outputs))))
        (clim:draw-text* pane text 2 0)))))

(defun display-not-found (pane info)
  (destructuring-bind (error-block error-transaction)
      info
    (let ((text (format nil
                        "~%No information found~
                         ~%~%Block request: ~a~
                         ~%~%Transaction request: ~a"
                        error-block error-transaction)))
      (clim:draw-text* pane text 2 0))))

(defun display-result (frame stream)
  (declare (ignore stream))
  (let* ((info (query-result frame))
         (result (clim:find-pane-named frame 'result)))
    (case (car info)
      ((:block) (display-block result (cdr info)))
      ((:transaction) (display-transaction result (cdr info)))
      ((:error) (display-not-found result (cdr info))))))

(defun gui ()
  (let ((frame (clim:make-application-frame 'explorer-frame
                                            :pretty-name "Monero Explorer")))
    (clim:run-frame-top-level frame)))
