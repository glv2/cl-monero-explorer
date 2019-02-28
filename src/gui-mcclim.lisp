;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-explorer-mcclim
  (:use :cl)
  (:import-from :monero-explorer-common
                #:lookup-block
                #:lookup-transaction)
  (:import-from :monero-tools-daemon-rpc
                #:*rpc-host*
                #:*rpc-port*)
  (:export #:gui))

(in-package :monero-explorer-mcclim)


(clim:define-application-frame explorer-frame ()
  ((query-result :initform '(:empty) :accessor query-result))
  (:menu-bar nil)
  (:panes
   (host-label :label :label "Host" :max-width 50)
   (host :text-field :value "127.0.0.1")
   (port-label :label :label "Port" :max-width 50)
   (port :text-field :value "18081" :width 100 :max-width 100)
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
                    port-label port)))
              (clim:spacing (:thickness 5)
                (clim:horizontally (:width 950 :spacing 5)
                  (clim:spacing (:thickness 5) query)
                  lookup))
              result))))

(defun lookup (pane)
  (let* ((frame (clim:pane-frame pane))
         (*rpc-host* (clim:gadget-value (clim:find-pane-named frame 'host)))
         (*rpc-port* (clim:gadget-value (clim:find-pane-named frame 'port)))
         (query (clim:gadget-value (clim:find-pane-named frame 'query))))
    (multiple-value-bind (height length) (parse-integer query :junk-allowed t)
      (let* ((id (if (= length (length query)) height query))
             (info (or (ignore-errors (lookup-block id))
                       (ignore-errors (lookup-transaction id)))))
        (setf (query-result frame) info)
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
                       amount key))))
      (let ((text (format nil
                          "~%Block height: ~d~
                           ~%Block timestamp: ~d~
                           ~%Version: ~d~
                           ~%In pool: ~:[No~;Yes~]~
                           ~%Double spend seen: ~:[No~;Yes~]~
                           ~%Unlock time: ~d~
                           ~2%Extra: ~a~
                           ~2%Inputs: ~d~
                           ~%~{~a~%~}~
                           ~%Outputs: ~d~
                           ~%~{~a~%~}"
                          block-height block-timestamp version in-pool
                          double-spend-seen unlock-time extra
                          (length inputs)
                          (mapcar #'print-input inputs)
                          (length outputs)
                          (mapcar #'print-output outputs))))
        (clim:draw-text* pane text 2 0)))))

(defun display-not-found (pane)
  (let ((text (format nil "~%No information found")))
    (clim:draw-text* pane text 2 0)))

(defun display-result (frame stream)
  (declare (ignore stream))
  (let* ((info (query-result frame))
         (result (clim:find-pane-named frame 'result)))
    (case (car info)
      ((:block) (display-block result (cdr info)))
      ((:transaction) (display-transaction result (cdr info)))
      ((:empty))
      (t (display-not-found result)))))

(defun gui ()
  (let ((frame (clim:make-application-frame 'explorer-frame
                                            :pretty-name "Monero Explorer")))
    (clim:run-frame-top-level frame)))
