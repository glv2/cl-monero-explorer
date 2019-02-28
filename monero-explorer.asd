;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

(defsystem "monero-explorer"
  :name "monero-explorer"
  :description "Blockchain explorer for the Monero crypto-currency"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("ltk"
               "mcclim"
               "monero-tools"
               "monero-tools-rpc")
  :components ((:module "src"
                :components ((:file "gui-ltk" :depends-on ("lookup" "package"))
                             (:file "gui-mcclim" :depends-on ("lookup" "package"))
                             (:file "lookup" :depends-on ("package"))
                             (:file "package")))))
