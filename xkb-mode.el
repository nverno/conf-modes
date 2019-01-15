;;; xkb-mode.el --- major mode for XKB configs -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-mode
;; Last modified: <2019-01-15 00:42:07>
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
;; old, but has a bunch: https://github.com/vim/vim/runtime/syntax/xkb.vim
(defvar xkb-font-lock-keywords
  (eval-when-compile
    (let* ((preproc '("augment" "include" "replace"))
           (modif '("override" "replace"))
           (tmodif '("default" "hidden" "partial" "virtual"))
           (kws '("xkbIdentifier" "action" "actions" "affect" "alias"
                  "allowExplicit" "approx" "baseColor" "button" "clearLocks"
                  "color" "controls" "cornerRadius" "count" "ctrls"
                  "description" "driveskbd" "font" "fontSize" "gap" "group"
                  "groups" "height" "indicator" "indicatorDrivesKeyboard"
                  "interpret" "key" "keys" "labelColor" "latchToLock"
                  "latchMods" "left" "level_name" "map" "maximum" "minimum"
                  "modifier_map" "modifiers" "name" "offColor" "onColor"
                  "outline" "preserve" "priority" "right" "repeat" "row" "section"
                  "section" "setMods" "shape" "slant" "solid" "symbols" "text"
                  "top" "type" "useModMapMods" "virtualModifier" "virtualMods"
                  "virtual_modifiers" "weight" "whichModState" "width"))
           (funcs '("xkbFunction" "AnyOf" "ISOLock" "LatchGroup" "LatchMods"
                    "LockControls" "LockGroup" "LockMods" "LockPointerButton"
                    "MovePtr" "NoAction" "PointerButton" "SetControls"
                    "SetGroup" "SetMods" "SetPtrDflt" "Terminate"))
           (sect '("alphanumeric_keys" "alternate_group" "function_keys"
                   "keypad_keys" "modifier_keys" "xkb_compatibility"
                   "xkb_geometry" "xkb_keycodes" "xkb_keymap" "xkb_semantics"
                   "xkb_symbols" "xkb_types")))
      `(("<\\([^>]+\\)>" (1 font-lock-variable-name-face))
        ("\\([A-Za-z._0-9]+\\)[\]\[0-9:]*\\([0-9A-Za-z._]*\\)\\s-*="
         (1 font-lock-variable-name-face)
         (2 font-lock-variable-name-face))
        (,(re-opt (append preproc modif tmodif)) (1 font-lock-preprocessor-face))
        (,(re-opt kws) (1 font-lock-keyword-face))
        (,(re-opt funcs) (1 font-lock-function-name-face))
        (,(re-opt sect) (1 font-lock-type-face))
        ;; special thingies
        ("\\(?:true\\|false\\|[-*+!:]\\)" . font-lock-constant-face)))))

(defvar xkb-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?_ "w" st)
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

;; (defvar xkb-imenu-expression
;;   '((nil )))

;;;###autoload
(define-derived-mode xkb-mode  prog-mode "xkb"
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(xkb-font-lock-keywords nil nil nil))
  (smie-setup xkb-smie-grammar #'xkb-smie-rules))

;;;###autoload
(add-to-list 'auto-mode-alist '("/xkb/.*" . xkb-mode))

(provide 'xkb-mode)
;;; xkb-mode.el ends here
