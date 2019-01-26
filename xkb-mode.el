;;; xkb-mode.el --- major mode for XKB configs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Created:  7 November 2016
;; Last modified: <2019-01-26 03:30:56>
;; URL: https://github.com/nverno/conf-mode
;; Package-Requires: 

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

;; Major mode for XKB files: syntax, font-lock, and indentation
;; Vim syntax: https://github.com/vim/vim/tree/master/runtime/syntax

;;; Code:
(require 'smie)
(eval-when-compile
  (defmacro re-opt (opts)
    `(concat "\\_<" (regexp-opt ,opts t) "\\_>")))

(defvar xkb-indent-offset 4 "Default indentaion offset for `xkb-mode'.")

;; probably missing stuff
(defvar xkb-font-lock-keywords
  (eval-when-compile
    (let* ((preproc '("augment" "include" "replace"))
           (modif '("override" "replace"))
           (tmodif '("default" "hidden" "partial" "virtual"))
           (kws '("accel" "accelerate" "action" "actions" "affect"
                  "alias" "allowExplicit" "approx" "baseColor" "button"
                  "clearLocks" "clearModifiers" "clearMods" "color" "controls"
                  "cornerRadius" "count" "ctrls" "data" "description" "device"
                  "driveskbd" "font" "fontSize" "gap" "generateKeyEvent"
                  "genKeyEvent" "group" "groups" "height" "indicator"
                  "indicatorDrivesKeyboard" "interpret" "key" "keyCode"
                  "keys" "labelColor" "latchMods" "latchToLock" "left"
                  "level_name" "map" "maximum" "minimum" "modifier_map" "modifiers"
                  "name" "offColor" "onColor" "outline" "preserve" "priority"
                  "repeat" "report" "right" "row" "same" "sameServer" "screen"
                  "section" "setMods" "shape" "slant" "solid" "symbols" "text"
                  "top" "type" "useModMapMods" "value" "virtual_modifiers"
                  "virtualModifier" "virtualMods" "weight" "whichModState"
                  "width" "xkbIdentifier"))
           (funcs '("ActionMessage" "AllOf" "AnyOf" "AnyOfOrNone" "DeviceButton"
                    "DeviceValuator" "Exactly" "ISOLock" "LatchGroup"
                    "LatchMods" "LockControls" "LockDeviceButton" "LockGroup"
                    "LockMods" "LockPointerButton" "Message" "MessageAction"
                    "MovePtr" "NoAction" "PointerButton" "Private" "RedirectKey"
                    "SetControls" "SetGroup" "SetMods" "SetPointerDefault"
                    "SetPtrDflt" "SwitchScreen" "Terminate" "TerminateServer"
                    "xkbFunction"))
           (sect '("alphanumeric_keys" "alternate_group" "function_keys"
                   "keypad_keys" "modifier_keys" "xkb_compatibility"
                   "xkb_geometry" "xkb_keycodes" "xkb_keymap" "xkb_semantics"
                   "xkb_symbols" "xkb_types")))
      `(("<\\([^>]+\\)>" (1 font-lock-variable-name-face))
        (,(re-opt (append preproc modif tmodif)) (1 font-lock-preprocessor-face))
        (,(re-opt kws) (1 font-lock-keyword-face))
        (,(re-opt funcs) (1 font-lock-function-name-face))
        (,(re-opt sect) (1 font-lock-type-face))
        ("\\([A-Za-z._0-9+]*\\)[\]\[0-9:]*\\([+0-9A-Za-z._]*\\)\\s-*="
         (1 font-lock-variable-name-face)
         (2 font-lock-variable-name-face))
        ;; special thingies
        ("\\(?:[Tt]rue\\|[Ff]alse\\|[-*+!:]\\)" . font-lock-constant-face)))))

(defvar xkb-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?= "." st)
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

(defvar xkb-imenu-expression
  '((nil "xkb_\\([a-z]+\\s-+[^{\n \t]*\\)" 1)))

;;;###autoload
(define-derived-mode xkb-mode  prog-mode "xkb"
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(xkb-font-lock-keywords))
  (setq imenu-generic-expression xkb-imenu-expression)
  (smie-setup xkb-smie-grammar #'xkb-smie-rules))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\(/xkb/.*\\|\.[Xx]kb\\(?:map\\)?\\'\\)" . xkb-mode))

(provide 'xkb-mode)
;;; xkb-mode.el ends here
