;;; netrc-mode.el --- major mode for .netrc files -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Last modified: <2019-01-16 17:39:38>
;; Package-Requires: 
;; Created: 13 December 2018

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

;; Major mode for .netrc files: syntax, font-locking, and indentation

;;; Code:

(require 'authinfo-mode)
(require 'smie)

(defvar netrc-mode-indent-offset 8
  "Indentation offset for `netrc-mode', defaults to align with \"machine\".")

(defconst netrc-mode-smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2
    '((assoc "machine") (assoc "\n")))))

(defun netrc-mode-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic))
    (`(:elem . args) (smie-indent-keyword "\n"))
    (`(:before . "\n")
     (- netrc-mode-indent-offset (current-column)))))

;;;###autoload
(define-derived-mode netrc-mode authinfo-mode
  (modify-syntax-entry ?\. "_")
  (smie-setup netrc-mode-smie-grammar #'netrc-mode-smie-rules))

;;;###autoload
(add-hook 'auto-mode-alist '("[_.]netrc\\(\.gpg\\)?\\'" . netrc-mode))

(provide 'netrc-mode)
;;; netrc-mode.el ends here
