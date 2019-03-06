;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

#+(and sbcl sb-core-compression)
(defmethod perform ((o program-op) (c system))
  (uiop:dump-image (output-file o c) :executable t :compression t))

(defsystem "monero-explorer-ltk"
  :name "monero-explorer-ltk"
  :description "Blockchain explorer for the Monero crypto-currency (LTK GUI)"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("ltk"
               "monero-explorer-common"
               "monero-tools-rpc")
  :build-operation "program-op"
  :build-pathname "monero-explorer-ltk"
  :entry-point "monero-explorer-ltk:gui"
  :components ((:module "src"
                :components ((:file "gui-ltk")))))
