;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-explorer-common
  (:use :cl)
  (:import-from :monero-tools
                #:deserialize-transaction-prefix)
  (:import-from :monero-daemon-rpc
                #:get-block
                #:get-transactions)
  (:import-from :monero-utils
                #:bytes->hex-string
                #:geta
                #:hex-string->bytes)
  (:export #:lookup-block
           #:lookup-transaction))

(in-package :monero-explorer-common)


(defun lookup-block (id)
  (let* ((result (get-block id))
         (block-header (geta result :block-header)))
    (when block-header
      (list :block
            (geta block-header :block-size)
            (geta block-header :hash)
            (geta block-header :height)
            (geta block-header :major-version)
            (geta block-header :minor-version)
            (geta block-header :nonce)
            (geta block-header :prev-hash)
            (geta block-header :reward)
            (geta block-header :timestamp)
            (geta block-header :miner-tx-hash)
            (geta result :tx-hashes)))))

(defun lookup-transaction (id)
  (let* ((result (get-transactions (list id) :prune t))
         (transaction (car (geta result :txs))))
    (when transaction
      (let* ((hex (geta transaction :pruned-as-hex))
             (bin (hex-string->bytes hex))
             (prefix (deserialize-transaction-prefix bin 0)))
        (list :transaction
              (geta transaction :block-height)
              (geta transaction :block-timestamp)
              (geta transaction :double-spend-seen)
              (geta transaction :in-pool)
              (geta prefix :version)
              (geta prefix :unlock-time)
              (remove nil
                      (map 'list
                           (lambda (input)
                             (let ((k (geta input :key)))
                               (when k
                                 (list (geta k :amount)
                                       (coerce (geta k :key-offsets) 'list)
                                       (bytes->hex-string (geta k :key-image))))))
                           (geta prefix :inputs)))
              (map 'list
                   (lambda (output)
                     (list (geta output :amount)
                           (bytes->hex-string (geta (geta output :target) :key))))
                   (geta prefix :outputs))
              (let* ((extra (mapcar #'car (geta prefix :extra)))
                     (transaction-public-key (geta extra :transaction-public-key))
                     (additional-public-keys (geta extra :additional-public-keys))
                     (nonce (geta extra :nonce))
                     (payment-id (geta nonce :payment-id))
                     (encrypted-payment-id (geta nonce :encrypted-payment-id)))
                (list (bytes->hex-string transaction-public-key)
                      (map 'list #'bytes->hex-string additional-public-keys)
                      (if payment-id
                          (bytes->hex-string payment-id)
                          (bytes->hex-string encrypted-payment-id)))))))))
