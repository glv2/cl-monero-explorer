;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-explorer-common
  (:use :cl)
  (:import-from :monero-tools
                #:bytes->hex-string
                #:deserialize-transaction-prefix
                #:geta
                #:hex-string->bytes)
  (:import-from :monero-tools-daemon-rpc
                #:get-block
                #:get-transactions)
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
            (geta result :miner-tx-hash)
            (geta result :tx-hashes)))))

(defun lookup-transaction (id)
  (let* ((result (get-transactions (list id) :prune t))
         (transaction (car (geta result :txs))))
    (when transaction
      (let* ((hex (geta transaction :as-hex))
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
              (geta prefix :extra))))))
