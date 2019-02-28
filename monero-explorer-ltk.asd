;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

(defsystem "monero-explorer-ltk"
  :name "monero-explorer-ltk"
  :description "Blockchain explorer for the Monero crypto-currency (LTK GUI)"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("ltk"
               "monero-explorer-common"
               "monero-tools-rpc")
  :components ((:module "src"
                :components ((:file "gui-ltk")))))