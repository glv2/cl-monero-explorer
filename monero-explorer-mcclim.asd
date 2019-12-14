;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

#+(and sbcl sb-core-compression)
(defmethod perform ((o program-op) (c system))
  (uiop:dump-image (output-file o c) :executable t :compression t))

(defsystem "monero-explorer-mcclim"
  :name "monero-explorer-mcclim"
  :description "Blockchain explorer for the Monero crypto-currency (McCLIM GUI)"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("mcclim"
               "monero-explorer-common"
               "monero-rpc")
  :build-operation "program-op"
  :build-pathname "monero-explorer-mcclim"
  :entry-point "monero-explorer-mcclim:gui"
  :components ((:module "src"
                :components ((:file "gui-mcclim")))))
