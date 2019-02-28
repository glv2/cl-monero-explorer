;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-explorer)


(defun display-block-ltk (result info)
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

(defun display-transaction-ltk (result info)
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
        (ltk:clear-text result)
        (ltk:append-text result text)))))

(defun display-not-found-ltk (result)
  (ltk:clear-text result)
  (ltk:append-text result "No information found"))

(defun gui-ltk ()
  (declare (optimize (debug 3)))
  (ltk:with-ltk ()
    (flet ((lookup (host port query result)
             (let* ((*rpc-host* (ltk:text host))
                    (*rpc-port* (ltk:text port))
                    (query (ltk:text query)))
               (multiple-value-bind (height length)
                   (parse-integer query :junk-allowed t)
                 (let* ((id (if (= length (length query)) height query))
                        (info (or (ignore-errors (lookup-block id))
                                  (ignore-errors (lookup-transaction id)))))
                   (case (car info)
                     ((:block)
                      (display-block-ltk result (cdr info)))
                     ((:transaction)
                      (display-transaction-ltk result (cdr info)))
                     (t
                      (display-not-found-ltk result))))))))
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
             (frame2 (make-instance 'ltk:frame))
             (query (make-instance 'ltk:entry
                                   :master frame2
                                   :text "Enter hash or height"))
             (result (make-instance 'ltk:text
                                    :width 100))
             (lookup (make-instance 'ltk:button
                                    :master frame2
                                    :text "Lookup"
                                    :command (lambda ()
                                               (lookup host port query result)))))
        (ltk:format-wish "wm title \".\" {Monero Explorer}")
        (ltk:bind query "<Return>" (lambda (event)
                                     (declare (ignore event))
                                     (lookup host port query result)))
        (ltk:pack frame1 :expand t :fill :x :pady 5)
        (ltk:pack host-label :side :left :padx 5)
        (ltk:pack host :side :left :expand t :fill :x :padx 5)
        (ltk:pack port-label :side :left :padx 5)
        (ltk:pack port :side :left :padx 5)
        (ltk:pack frame2 :expand t :fill :x :pady 5)
        (ltk:pack query :side :left :expand t :fill :x :padx 5)
        (ltk:pack lookup :side :left :padx 5)
        (ltk:pack result)))))
