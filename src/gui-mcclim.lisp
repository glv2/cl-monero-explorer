;;;; This file is part of monero-explorer
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-explorer)


(clim:define-application-frame explorer-frame ()
  ((query-result :initform '(:empty) :accessor query-result))
  (:menu-bar nil)
  (:panes
   (query :text-field
          :value "da6ce65f5e0f6e6d46b15fe39613075aba744eb8e30bfee8dd44d9019a941ea1"
          :activate-callback #'lookup)
   (lookup :push-button
           :max-width 50
           :label "Lookup"
           :activate-callback #'lookup)
   (result :application
           :scroll-bars :both
           :display-function #'display-result))
  (:layouts
   (default (clim:vertically (:height 800)
              (clim:spacing (:thickness 5)
                (clim:horizontally (:width 950 :spacing 5)
                  (clim:spacing (:thickness 5) query)
                  lookup))
              result))))

(defun lookup (pane)
  (let* ((frame (clim:pane-frame pane))
         (query (clim:gadget-value (clim:find-pane-named frame 'query))))
    (multiple-value-bind (height length) (parse-integer query :junk-allowed t)
      (let* ((id (if (= length (length query)) height query))
             (info (or (ignore-errors (lookup-block id))
                       (ignore-errors (lookup-transaction id)))))
        (setf (query-result frame) info)
        (clim:redisplay-frame-pane frame 'result)))))

(defun display-block (pane info)
  (destructuring-bind (block-size hash height major-version minor-version
                       nonce prev-hash reward timestamp
                       miner-tx-hash tx-hashes)
      info
    (let* ((text (format nil "
Block: ~a
Height: ~d
Previous block: ~a
Version: ~d.~d
Size: ~d
Nonce: ~d
Timestamp: ~d
Reward: ~d
Miner transaction: ~a
Transactions: ~%~{~4t~a~%~}
"
                         hash height prev-hash major-version minor-version
                         block-size nonce timestamp reward miner-tx-hash
                         tx-hashes)))
           ;; (rect (clim:bounding-rectangle (clim:sheet-region pane)))
           ;; (width (clim:rectangle-width rect))
           ;; (height (clim:rectangle-height rect)))
      ;; (clim:draw-rectangle* pane 0 0 width (floor height 2) :filled nil)
      (clim:draw-text* pane text 2 0))))

(defun display-transaction (pane info)
  (destructuring-bind (block-height block-timestamp double-spend-seen
                       in-pool version unlock-time inputs outputs extra)
      info
    (let ((text (format nil "
Block height: ~d
Block timestamp: ~d
Version: ~d
In pool: ~:[No~;Yes~]
Double spend seen: ~:[No~;Yes~]
Unlock time: ~d
Extra: ~a
Inputs: ~d
~a
Outputs: ~d
~a
"
                        block-height block-timestamp version in-pool
                        double-spend-seen unlock-time extra
                        (length inputs) inputs
                        (length outputs) outputs)))
      (clim:draw-text* pane text 2 0))))

(defun display-not-found (pane)
  (let ((text (format nil "~%No information found")))
    (clim:draw-text* pane text 2 0)))

(defun display-result (frame stream)
  (declare (ignore stream))
  (let* ((info (query-result frame))
         (result (clim:find-pane-named frame 'result)))
    (case (car info)
      ((:block) (display-block result (cdr info)))
      ((:transaction) (display-transaction result (cdr info)))
      ((:empty))
      (t (display-not-found result)))))

(defun gui ()
  (let ((frame (clim:make-application-frame 'explorer-frame
                                            :pretty-name "Monero Explorer")))
    (clim:run-frame-top-level frame)))
