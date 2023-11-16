;;; clang-tidy-mode.el --- Major mode for clang-tidy configuration -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Created: 15 November 2023
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
;; Major mode for clang-tidy config.
;; Provides `completion-at-point' for checks.
;;; Code:

(require 'conf-mode)
(require 'conf-completion)

(defun clang-tidy-mode--update-keywords (kws)
  (unless (length> clang-tidy-mode-font-lock-keywords 1)
    (let ((kws-re
           (concat "\\_<-?" (regexp-opt (mapcar #'substring-no-properties kws))
                   "\\_>")))
      (push `(,kws-re . font-lock-builtin-face) clang-tidy-mode-font-lock-keywords))
    (font-lock-refresh-defaults)
    (font-lock-flush)
    (font-lock-fontify-buffer)))

(defvar clang-tidy-mode-font-lock-keywords
  '(("^\\s-*\\(\\w+\\):" 1 'font-lock-property-name-face)))

;;;###autoload
(define-derived-mode clang-tidy-mode conf-mode "ClangTidy"
  "Major mode for .clang-tidy config."
  :group 'conf
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?# "< ")
  (conf-mode-initialize "#")
  (setq-local comment-end "")
  (setq-local conf-completion-skip-leading-regexp "[-]")
  (setq-local font-lock-defaults '(clang-tidy-mode-font-lock-keywords))
  (conf-completion-initialize "clang-tidy" #'clang-tidy-mode--update-keywords))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.clang-tidy\\'" . clang-tidy-mode))

(provide 'clang-tidy-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; clang-tidy-mode.el ends here
