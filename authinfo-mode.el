;;; authinfo-mode ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: 
;; Created: 12 December 2018

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
(define-derived-mode authinfo-mode fundamental-mode "Authinfo"
  (setq-local font-lock-defaults '(authinfo-mode-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

;;;###autoload
(add-hook 'auto-mode-alist '("[_.]authinfo\\(\\.gpg\\)?\\'" . authinfo-mode))

(provide 'authinfo-mode)
;;; authinfo-mode.el ends here
