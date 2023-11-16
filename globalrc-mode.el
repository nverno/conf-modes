;;; globalrc-mode.el --- Major mode for GNU global config -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: ((emacs "24.3"))
;; Created: 25 October 2019
;; Version: 0.1.0
;; Keywords: languages

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
;; Major mode for global .globalrc configs.
;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'conf-mode)
(require 'dotctags-mode)

(defconst globalrc-font-lock-keywords
  `(("^[^: \t\n][^:\n]*:"   . font-lock-constant-face)
    ("\\(\$[[:alnum:]]+\\)" . font-lock-variable-name-face)
    ,@conf-font-lock-keywords
    ("\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\\\)$" 3 'font-lock-string-face)
    (":"                    . font-lock-builtin-face)))

;;;###autoload
(define-derived-mode globalrc-mode conf-windows-mode "Conf[global]"
  "Major mode for .globalrc confgs."
  :group 'conf
  :syntax-table dotctags-mode-syntax-table
  (conf-mode-initialize "#" 'globalrc-font-lock-keywords)
  (setq-local comment-end "")
  (setq-local syntax-propertize-function #'dotctags-propertize)
  (setq-local imenu-generic-expression '((nil "^[^: #\t\n|]+" 0))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.globalrc\\'" . globalrc-mode))

(provide 'globalrc-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; globalrc-mode.el ends here
