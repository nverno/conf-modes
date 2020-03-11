;;; npmrc-mode.el --- Major mode for .npmrc -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Last modified: <2019-01-16 17:40:51>
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
;;
;; Major mode and completion for .npmrc / .yarnrc
;;
;;; Code:
(eval-when-compile (require 'subr-x))
(require 'conf-mode)
(require 'conf-completion)

(defvar npmrc-package-managers
  `((".npmrc"
     .
     ((program . "npm")
      (args . ("config" "ls" "-l"))
      (regexp . ("^\\([^; \t]+\\)\\s-*=\\s-*\\(.*\\)" 1 2))))
    (".yarnrc"
     .
     ((program . "yarn")
      (shell-command
       . 
       ,(concat
         "yarn config --no-default-rc --json list | head -n2 | tail -n1 |"
         "jq -M '.data| keys | join(\" \")'"))
      (regexp . ("[^ ]+"))))))

;; initialize completion variables based on package manager
(defun npmrc-mode-initialize-completion ()
  (let ((fname (file-name-nondirectory (or (buffer-file-name) (buffer-name)))))
    (when-let ((vals (cdr (assoc fname npmrc-package-managers))))
      (pcase-dolist (`(,k . ,v) vals)
        (setq k (intern (concat "conf-completion-" (symbol-name k))))
        (set (make-local-variable k) v)))))

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
  (npmrc-mode-initialize-completion)
  (add-hook 'completion-at-point-functions #'conf-completion-at-point nil t))

;;;###autoload
(add-to-list
 'auto-mode-alist
 (cons (concat "\\." (regexp-opt '("npmrc" "yarnrc")) "\\'") 'npmrc-mode))

(provide 'npmrc-mode)
;;; npmrc-mode.el ends here
