;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-explorer-gtk
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

(in-package :monero-explorer-gtk)


(defun display-block (result info)
  (destructuring-bind (block-size hash height major-version minor-version
                       nonce prev-hash reward timestamp
                       miner-tx-hash tx-hashes)
      info
    (let ((buffer (gtk:gtk-text-view-buffer result))
          (text (format nil
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
      (setf (gtk:gtk-text-buffer-text buffer) text))))

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
      (let ((buffer (gtk:gtk-text-view-buffer result))
            (text (format nil
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
        (setf (gtk:gtk-text-buffer-text buffer) text)))))

(defun display-not-found (result error-block error-transaction)
  (let ((buffer (gtk:gtk-text-view-buffer result))
        (text (format nil
                      "~%No information found~
                       ~%~%Block request: ~a~
                       ~%~%Transaction request: ~a"
                      error-block error-transaction)))
    (setf (gtk:gtk-text-buffer-text buffer) text)))

(defun lookup (host port user password query result)
  (let ((*rpc-host* (gtk:gtk-entry-text host))
        (*rpc-port* (gtk:gtk-entry-text port))
        (*rpc-user* (gtk:gtk-entry-text user))
        (*rpc-password* (gtk:gtk-entry-text password))
        (query (gtk:gtk-entry-text query)))
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

(defun gui ()
  (gtk:within-main-loop
    (let* ((toplevel (make-instance 'gtk:gtk-window
                                    :type :toplevel
                                    :title "Monero Explorer"))
           (vbox (make-instance 'gtk:gtk-vbox
                                :parent toplevel
                                :margin 5))
           (hbox1 (make-instance 'gtk:gtk-hbox
                                 :parent vbox
                                 :margin-bottom 10))
           (host-label (make-instance 'gtk:gtk-label
                                      :parent hbox1
                                      :label "Host:"
                                      :margin-right 5))
           (host (make-instance 'gtk:gtk-entry
                                :parent hbox1
                                :text "127.0.0.1"
                                :margin-right 5))
           (port-label (make-instance 'gtk:gtk-label
                                      :parent hbox1
                                      :label "Port:"
                                      :margin-left 5
                                      :margin-right 5))
           (port (make-instance 'gtk:gtk-entry
                                :parent hbox1
                                :text "18081"
                                :margin-right 5))
           (user-label (make-instance 'gtk:gtk-label
                                      :parent hbox1
                                      :label "User:"
                                      :margin-left 5
                                      :margin-right 5))
           (user (make-instance 'gtk:gtk-entry
                                :parent hbox1
                                :text ""
                                :margin-right 5))
           (password-label (make-instance 'gtk:gtk-label
                                          :parent hbox1
                                          :label "Password:"
                                          :margin-left 5
                                          :margin-right 5))
           (password (make-instance 'gtk:gtk-entry
                                    :parent hbox1
                                    :text ""
                                    :visibility nil))
           (hbox2 (make-instance 'gtk:gtk-hbox
                                 :parent vbox
                                 :margin-bottom 10))
           (query (make-instance 'gtk:gtk-entry
                                 :parent hbox2
                                 :text "Enter hash or height"
                                 :expand t
                                 :width-request 500
                                 :margin-right 10))
           (lookup (make-instance 'gtk:gtk-button
                                  :parent hbox2
                                  :label "Lookup"
                                  :expand nil
                                  :width-request 50))
           (scrolled (make-instance 'gtk:gtk-scrolled-window
                                    :parent toplevel
                                    :height-request 500))
           (result (make-instance 'gtk:gtk-text-view
                                  :parent scrolled
                                  :editable nil
                                  :hexpand t
                                  :vexpand t)))
      (gobject:g-signal-connect toplevel
                                "destroy"
                                (lambda (widget)
                                  (declare (ignore widget))
                                  (gtk:leave-gtk-main)))
      (let ((lkp (lambda (widget)
                   (declare (ignore widget))
                   (lookup host port user password query result))))
        (gobject:g-signal-connect lookup "clicked" lkp)
        (gobject:g-signal-connect query "activate" lkp))
      (gtk:gtk-container-add hbox1 host-label)
      (gtk:gtk-container-add hbox1 host)
      (gtk:gtk-container-add hbox1 port-label)
      (gtk:gtk-container-add hbox1 port)
      (gtk:gtk-container-add hbox1 user-label)
      (gtk:gtk-container-add hbox1 user)
      (gtk:gtk-container-add hbox1 password-label)
      (gtk:gtk-container-add hbox1 password)
      (gtk:gtk-container-add hbox2 query)
      (gtk:gtk-container-add hbox2 lookup)
      (gtk:gtk-container-add vbox hbox1)
      (gtk:gtk-container-add vbox hbox2)
      (gtk:gtk-container-add scrolled result)
      (gtk:gtk-container-add vbox scrolled)
      (gtk:gtk-widget-show-all toplevel)))
  (gtk:join-gtk-main))
