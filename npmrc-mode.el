;;; npmrc-mode --- npmrc-mode -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: '(company)
;; Created: 17 August 2018

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
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'conf-macros))
(require 'company)
(require 'conf-mode)

;; -------------------------------------------------------------------
;;; Major mode

;; comment start with either [;#]
(defvar npmrc-mode-syntax-table
  (let ((tab (make-syntax-table conf-unix-mode-syntax-table)))
    (modify-syntax-entry ?\; "<" tab)
    tab))

;;;###autoload
(define-derived-mode npmrc-mode conf-unix-mode "Conf[npmrc]"
  "Conf mode for npmrc."
  :syntax-table npmrc-mode-syntax-table
  (conf-mode-initialize "#")
  (setq-local company-backends (cons 'company-npmrc company-backends)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.npmrc" . npmrc-mode))

;; -------------------------------------------------------------------
;;; Completion

;; create completion candidates from 'npm config --help'
(company-conf npmrc :program "npm" :args ("config" "--help")
              :keyword-re "\\s-*\\([^\[<]+\\)\\s-*\\([^\n]*\\)")

(provide 'npmrc-mode)
;;; npmrc-mode.el ends here
