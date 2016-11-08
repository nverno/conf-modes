;;; xkb-mode --- major mode for xkb keys

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-mode
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created:  7 November 2016

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

;; Major mode for xbk configuration files:
;; Does the basics: indentation / font-lock / syntax etc.

;;; Code:
(require 'smie)
(eval-when-compile
  (defmacro re-opt (opts)
    `(concat "\\_<" (regexp-opt ,opts t) "\\_>")))

(defvar xkb-indent-offset 4 "Default indentaion offset for `xkb-mode'.")

;; probably missing lots, just added some
(defvar xkb-font-lock-keywords
  (eval-when-compile
    (let ((variants 
           '("default" "partial" "hidden" "alphanumeric_keys"
             "modifier_keys" "keypad_keys" "function_keys"
             "virtual_modifiers" "alternate_group"))
          (xkb '("xkb_symbols" "xkb_geometry" "xkb_compatibility"
                 "xkb_keycodes" "xkb_types"))
          (keywords '("key" "keys" "top" "left" "right" "interpret"
                      "group" "indicator" "row" "include" "name" "type"
                      "text" "outline" "solid" "augment" "section" "shape"
                      "override" "preserve" "map" "level_name" "alias")))
      `(("<\\([^>]+\\)>" (1 font-lock-variable-name-face))
        ("\\([A-Za-z.]+\\)\\s-*=" (1 font-lock-keyword-face))
        (,(re-opt variants) (1 font-lock-function-name-face))
        (,(re-opt xkb) (1 font-lock-builtin-face))
        (,(re-opt keywords) (1 font-lock-keyword-face))))))

(defvar xkb-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?_ "." st)
    st))

(defconst xkb-smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2
    '((assoc " ") (assoc "\n" ";")))))

(defun xkb-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) xkb-indent-offset)
    (`(:elem . args) 0)
    (`(:before . "{") (smie-rule-parent))
    (`(:list-intro . ,(or `"\n" `"" `";" `",")) t)))

;;;###autoload
(define-derived-mode xkb-mode  prog-mode "xkb"
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local font-lock-defaults
              '(xkb-font-lock-keywords nil nil nil))
  (smie-setup xkb-smie-grammar #'xkb-smie-rules))

;;;###autoload
(add-to-list 'auto-mode-alist '("/xkb/*" . xkb-mode))

(provide 'xkb-mode)
;;; xkb-mode.el ends here
