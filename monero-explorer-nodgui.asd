;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

#+(and sbcl sb-core-compression)
(defmethod perform ((o program-op) (c system))
  (uiop:dump-image (output-file o c) :executable t :compression t))

(defsystem "monero-explorer-nodgui"
  :name "monero-explorer-nodgui"
  :description "Blockchain explorer for the Monero crypto-currency (nodgui GUI)"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("monero-explorer-common"
               "monero-rpc"
               "nodgui")
  :build-operation "program-op"
  :build-pathname "monero-explorer-nodgui"
  :entry-point "monero-explorer-nodgui:gui"
  :components ((:module "src"
                :components ((:file "gui-nodgui")))))
