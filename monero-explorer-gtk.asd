;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

#+(and sbcl sb-core-compression)
(defmethod perform ((o program-op) (c system))
  (uiop:dump-image (output-file o c) :executable t :compression t))

(defsystem "monero-explorer-gtk"
  :name "monero-explorer-gtk"
  :description "Blockchain explorer for the Monero crypto-currency (GTK GUI)"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("cl-cffi-gtk"
               "monero-explorer-common"
               "monero-rpc")
  :build-operation "program-op"
  :build-pathname "monero-explorer-gtk"
  :entry-point "monero-explorer-gtk:gui"
  :components ((:module "src"
                :components ((:file "gui-gtk")))))
