;;; authinfo-mode.el --- Major mode for authinfo configs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Created: 12 December 2018
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: ((emacs "25.1"))
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
;; Basic syntax / font-locking for .authinfo.
;;; Code:

(defvar authinfo-mode-keywords
  (eval-when-compile
    `((,(regexp-opt
         '("machine" "login" "password" "account" "default" "macdef" "force" "port"
           "protocol")
         'paren)
       . font-lock-keyword-face))))

(defvar authinfo-mode-syntax-table
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?# "<" tab)
    (modify-syntax-entry ?\n ">" tab)
    tab))
  
;;;###autoload
(define-derived-mode authinfo-mode prog-mode "Authinfo"
  "Major mode for authinfo configs."
  :group 'conf
  (setq-local font-lock-defaults '(authinfo-mode-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

;;;###autoload
(add-hook 'auto-mode-alist '("[_.]authinfo\\(\\.gpg\\)?\\'" . authinfo-mode))

(provide 'authinfo-mode)
;;; authinfo-mode.el ends here
