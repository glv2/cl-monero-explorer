;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-explorer
  (:use :cl)
  (:import-from :monero-tools
                #:bytes->hex-string
                #:deserialize-transaction-prefix
                #:geta
                #:hex-string->bytes)
  (:import-from :monero-tools-daemon-rpc
                #:*rpc-host*
                #:*rpc-port*
                #:get-block
                #:get-transactions)
  (:export #:gui
           #:gui-ltk))
