;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-explorer)


(clim:define-application-frame explorer-frame ()
  ((query-result :initform "" :accessor query-result))
  (:menu-bar nil)
  (:panes
   (query :text-field
          :width 650
          :value "Enter hash or height"
          :activate-callback #'lookup)
   (lookup :push-button
           :width 50
           :label "Lookup"
           :activate-callback #'lookup)
   (result :application
           :scroll-bars :both
           :display-function #'display-result))
  (:layouts
   (default (clim:vertically ()
              (clim:horizontally () query lookup)
              result))))

(defun format-block (id info)
  (destructuring-bind (block-size hash height major-version minor-version
                       nonce prev-hash reward timestamp
                       miner-tx-hash tx-hashes)
      info
    (format nil "
Block: ~a
Height: ~d
Size: ~d
Version: ~d.~d
Nonce: ~d
" hash height block-size major-version minor-version nonce)))

(defun format-transaction (id info)
  (destructuring-bind (block-height block-timestamp double-spend-seen
                       in-pool version unlock-time inputs outputs extra)
      info
    "TODO"))

(defun format-not-found (id)
  (format nil "~%No information found about ~a" id))

(defun format-result (query)
  (multiple-value-bind (height length) (parse-integer query :junk-allowed t)
    (let ((id (if (= length (length query)) height query)))
      (handler-case
          (let ((info (or (lookup-block id) (lookup-transaction id))))
            (cond
              ((null info) (format-not-found id))
              ((= (length info) 11) (format-block id info))
              (t (format-transaction id info))))
        (t (err)
          (format nil "~%~a" err))))))

(defun lookup (pane)
  (let* ((frame (clim:pane-frame pane))
         (query (clim:find-pane-named frame 'query)))
    (setf (query-result frame) (format-result (clim:gadget-value query)))
    (clim:redisplay-frame-pane frame 'result)))

(defun display-result (frame stream)
  (declare (ignore stream))
  (let* ((result (clim:find-pane-named frame 'result))
         (rect (clim:bounding-rectangle (clim:sheet-region result)))
         (width (clim:rectangle-width rect))
         (height (clim:rectangle-height rect))
         (x 2)
         (y 2))
    (clim:draw-text* result (query-result frame) x y)))

(defun gui ()
  (let ((frame (clim:make-application-frame 'explorer-frame
                                            :pretty-name "Monero Explorer")))
    (clim:run-frame-top-level frame)))
