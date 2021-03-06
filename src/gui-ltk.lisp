;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-explorer-ltk
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

(in-package :monero-explorer-ltk)


(defun display-block (result info)
  (destructuring-bind (block-size hash height major-version minor-version
                       nonce prev-hash reward timestamp
                       miner-tx-hash tx-hashes)
      info
    (let ((text (format nil
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
      (ltk:clear-text result)
      (ltk:append-text result text))))

(defun display-transaction (result info)
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
        (ltk:clear-text result)
        (ltk:append-text result text)))))

(defun display-not-found (result error-block error-transaction)
  (let ((text (format nil
                      "~%No information found~
                       ~%~%Block request: ~a~
                       ~%~%Transaction request: ~a"
                      error-block error-transaction)))
    (ltk:clear-text result)
    (ltk:append-text result text)))

(defun lookup (host port user password query result)
  (let ((*rpc-host* (ltk:text host))
        (*rpc-port* (ltk:text port))
        (*rpc-user* (ltk:text user))
        (*rpc-password* (ltk:text password))
        (query (ltk:text query)))
    (multiple-value-bind (height length)
        (parse-integer query :junk-allowed t)
      (let ((id (if (= length (length query)) height query)))
        (multiple-value-bind (info error1)
            (ignore-errors (lookup-block id))
          (if info
              (display-block result (cdr info))
              (multiple-value-bind (info error2)
                  (ignore-errors (lookup-transaction id))
                (if info
                    (display-transaction result (cdr info))
                    (display-not-found result error1 error2)))))))))

(defun set-title (title)
  (ltk:format-wish (format nil "wm title \".\" {~a}" title)))

(defun gui ()
  (ltk:with-ltk ()
    (let* ((frame1 (make-instance 'ltk:frame))
           (host-label (make-instance 'ltk:label
                                      :master frame1
                                      :text "Host:"))
           (host (make-instance 'ltk:entry
                                :master frame1
                                :text "127.0.0.1"))
           (port-label (make-instance 'ltk:label
                                      :master frame1
                                      :text "Port:"))
           (port (make-instance 'ltk:entry
                                :master frame1
                                :text "18081"))
           (user-label (make-instance 'ltk:label
                                      :master frame1
                                      :text "User:"))
           (user (make-instance 'ltk:entry
                                :master frame1
                                :text ""))
           (password-label (make-instance 'ltk:label
                                          :master frame1
                                          :text "Password:"))
           (password (make-instance 'ltk:entry
                                    :master frame1
                                    :text ""
                                    :show #\*))
           (frame2 (make-instance 'ltk:frame))
           (query (make-instance 'ltk:entry
                                 :master frame2
                                 :text "Enter hash or height"))
           (result (make-instance 'ltk:text :xscroll t :yscroll t))
           (lookup (make-instance 'ltk:button
                                  :master frame2
                                  :text "Lookup"
                                  :command (lambda ()
                                             (lookup host port
                                                     user password
                                                     query result)))))
      (set-title "Monero Explorer")
      (let ((lkp (lambda (event)
                   (declare (ignore event))
                   (lookup host port user password query result))))
        (ltk:bind query "<Return>" lkp)
        (ltk:bind query "<KP_Enter>" lkp))
      (ltk:pack frame1 :fill :x :pady 5)
      (ltk:pack host-label :side :left :padx 5)
      (ltk:pack host :side :left :expand t :fill :x :padx 5)
      (ltk:pack port-label :side :left :padx 5)
      (ltk:pack port :side :left :padx 5)
      (ltk:pack user-label :side :left :padx 5)
      (ltk:pack user :side :left :padx 5)
      (ltk:pack password-label :side :left :padx 5)
      (ltk:pack password :side :left :padx 5)
      (ltk:pack frame2 :fill :x :pady 5)
      (ltk:pack query :side :left :expand t :fill :x :padx 5)
      (ltk:pack lookup :side :left :padx 5)
      (ltk:pack result :expand t :fill :both))))
