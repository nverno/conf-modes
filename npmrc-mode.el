;;; npmrc-mode.el --- Major mode for .npmrc -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: '(company)
;; Created: 17 August 2018
;; Version: 0.1.0
;; Keywords:

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
;;
;; Major mode and completion for .npmrc / .yarnrc
;;
;;; Code:

(eval-when-compile (require 'subr-x))
(require 'conf-mode)
(require 'conf-completion)

;; comment start with either [;#]
(defvar npmrc-mode-syntax-table
  (let ((tab (make-syntax-table conf-unix-mode-syntax-table)))
    (modify-syntax-entry ?\; "<" tab)
    tab))

;;;###autoload
(define-derived-mode npmrc-mode conf-unix-mode "Conf[npmrc]"
  "Major mode for npmrc configs."
  :group 'conf
  :syntax-table npmrc-mode-syntax-table
  (conf-mode-initialize "#")
  (conf-completion-initialize))

;;;###autoload
(add-to-list
 'auto-mode-alist
 (cons (concat "\\." (regexp-opt '("npmrc" "yarnrc")) "\\'") 'npmrc-mode))

(provide 'npmrc-mode)
;;; npmrc-mode.el ends here
