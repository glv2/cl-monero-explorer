;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-explorer-qt
  (:use :cl+qt)
  (:import-from :monero-explorer-common
                #:lookup-block
                #:lookup-transaction)
  (:import-from :monero-tools-rpc
                #:*rpc-host*
                #:*rpc-password*
                #:*rpc-port*
                #:*rpc-user*)
  (:export #:gui))

(in-package :monero-explorer-qt)
(in-readtable :qtools)


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
      (setf (q+:text result) text))))

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
        (setf (q+:text result) text)))))

(defun display-not-found (result error-block error-transaction)
  (let ((text (format nil
                      "~%No information found~
                       ~%~%Block request: ~a~
                       ~%~%Transaction request: ~a"
                      error-block error-transaction)))
    (setf (q+:text result) text)))

(defun lookup (host port user password query result)
  (let ((*rpc-host* (q+:text host))
        (*rpc-port* (q+:text port))
        (*rpc-user* (q+:text user))
        (*rpc-password* (q+:text password))
        (query (q+:text query)))
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

(define-widget toplevel (QWidget)
  ())

(define-subwidget (toplevel host-label) (q+:make-qlabel "Host:" toplevel))

(define-subwidget (toplevel host) (q+:make-qlineedit "127.0.0.1" toplevel))

(define-subwidget (toplevel port-label) (q+:make-qlabel "Port:" toplevel))

(define-subwidget (toplevel port) (q+:make-qlineedit "18081" toplevel))

(define-subwidget (toplevel user-label) (q+:make-qlabel "User:" toplevel))

(define-subwidget (toplevel user) (q+:make-qlineedit toplevel))

(define-subwidget (toplevel password-label) (q+:make-qlabel "Password:" toplevel))

(define-subwidget (toplevel password) (q+:make-qlineedit toplevel)
  (setf (q+:echo-mode password) 2))

(define-subwidget (toplevel hbox1) (q+:make-qhboxlayout)
  (q+:add-widget hbox1 host-label)
  (q+:add-widget hbox1 host)
  (q+:add-widget hbox1 port-label)
  (q+:add-widget hbox1 port)
  (q+:add-widget hbox1 user-label)
  (q+:add-widget hbox1 user)
  (q+:add-widget hbox1 password-label)
  (q+:add-widget hbox1 password))

(define-subwidget (toplevel query) (q+:make-qlineedit toplevel)
  (setf (q+:placeholder-text query) "Enter hash or height"))

(define-subwidget (toplevel lookup) (q+:make-qpushbutton "Lookup" toplevel))

(define-subwidget (toplevel hbox2) (q+:make-qhboxlayout)
  (q+:add-widget hbox2 query)
  (q+:add-widget hbox2 lookup))

(define-subwidget (toplevel result) (q+:make-qtextedit toplevel)
  (setf (q+:minimum-height result) 500))

(define-subwidget (toplevel vbox) (q+:make-qvboxlayout toplevel)
  (setf (q+:window-title toplevel) "Monero Explorer")
  (q+:add-layout vbox hbox1)
  (q+:add-layout vbox hbox2)
  (q+:add-widget vbox result))

(define-slot (toplevel lookup) ()
  (declare (connected lookup (clicked)))
  (declare (connected query (return-pressed)))
  (lookup host port user password query result))

(defun gui ()
  (with-main-window (window (make-instance 'toplevel))))
