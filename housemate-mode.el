;;; housemate-mode ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: 
;; Created: 29 October 2018

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;; DSL for housemate system
;;; Code:
(eval-when-compile
  (require 'cl-lib))

(eval-when-compile
  (defmacro re-opt (opts)
    `(concat "\\_<" (regexp-opt ,opts t) "\\_>")))

(defvar housemate-mode-font-lock-keywords
  (eval-when-compile
    (let* ((leaders '("define" "add" "set" "show"))
           (keywords '("house" "room" "sensor" "appliance" "occupant"
                       "type" "configuration" "energy-use" "address" "status"
                       "value" "floor" "windows"))
           (types '("pet" "child" "adult" ;occupant types
                    "resting" "active"  ;occupant states
                    ;; room types
                    "kitchen" "closet" "diningroom" "livingroom" "hallway"
                    "bedroom" "familyroom" "garage" "bathroom"
                    ;; sensor types
                    "smoke_detector" "camera"
                    ;; appliance types
                    "thermostat" "window" "door" "light" "tv" "pandora"
                    "oven" "refrigerator" "ava")))
      `(("“.*”" . font-lock-string-face)
        (,(re-opt leaders) (1 font-lock-keyword-face))
        (,(re-opt keywords) (1 font-lock-builtin-face))
        (,(re-opt types) (1 font-lock-type-face))
        ("\\_<[0-9]+\\_>" . nil)
        ("\\_<[[:alpha:]][[:alnum:]]*_[[:alnum:]_]+\\_>" .
         font-lock-function-name-face)
        ("\\_<[[:alpha:]][[:alnum:]_]*\\(?::[[:alnum:]_]+\\(?::[[:alnum:]_]+\\)?\\)?" .
         font-lock-variable-name-face)))))

(defvar housemate-mode-syntax-table
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?: "w" tab)
    (modify-syntax-entry ?_ "w" tab)
    (modify-syntax-entry ?\n ">" tab)
    (modify-syntax-entry ?# "<" tab)
    (modify-syntax-entry ?\" "\"" tab)
    tab))

;;;###autoload
(define-derived-mode housemate-mode prog-mode "housemate"
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults
              '(housemate-mode-font-lock-keywords nil 'case-fold nil)))

(provide 'housemate-mode)
;;; housemate-mode.el ends here
