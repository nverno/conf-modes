;;; dotctags-mode.el --- Major mode and completion for .ctags -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Created: 29 September 2016
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: (("company"))
;; Keywords: languages tools matching

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

;; Major mode for .ctags configs and company completion backend. The completion
;; is configured for universal-ctags, not exuberant-ctags bundled with emacs.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'conf-mode)
(require 'conf-completion)

;; -------------------------------------------------------------------
;;; Major-mode

(defun dotctags-propertize (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("^\\s-*\\(#\\).*" (1 "<")))
   (point) end))

(defvar dotctags-mode-syntax-table
  (let ((st (make-syntax-table conf-windows-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    ;; (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?$ "'" st)
    st))

;;;###autoload
(define-derived-mode dotctags-mode conf-windows-mode "Conf[ctags]"
  "Conf Mode for (universal)-ctags config."
  :syntax-table dotctags-mode-syntax-table
  (conf-mode-initialize "#")
  (setq-local comment-end "")
  (setq-local company-backends '(company-dotctags))
  (setq-local syntax-propertize-function #'dotctags-propertize)
  (conf-completion-initialize "ctags"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ctags\\'" . dotctags-mode))

(provide 'dotctags-mode)
;;; dotctags-mode.el ends here
