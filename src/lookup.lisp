;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-explorer)


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
              ;; (map 'list
              ;;      (lambda (input)
              ;;        (let ((k (geta input :key)))
              ;;          (list (geta k :amount)
              ;;                (geta k :key-offsets)
              ;;                (geta k :key-image))))
              ;;      (geta prefix :inputs))
              (geta prefix :inputs)
              ;; (map 'list
              ;;      (lambda (output)
              ;;        (list (geta output :amount)
              ;;              (geta (geta output :target) :key)))
              ;;      (geta prefix :outputs))
              (geta prefix :outputs)
              (geta prefix :extra))))))

;; (defun get-transaction-pool ()
;;   (let ((result (monero-tools-daemon-rpc:get-transaction-pool-hashes)))
;;     (when result
;;       (geta result :tx-hashes))))
