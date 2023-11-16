;;; ccls-config-mode.el --- Major mode for .ccls config -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: ((emacs "29.1"))
;; Created:  4 December 2022
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
;;
;; Reference: https://github.com/MaskRay/ccls/wiki/Project-Setup#ccls-file
;;
;;; Code:

(eval-when-compile (require 'cl-lib))

(defvar ccls-config-mode-driver '("clang" "clang++"))

(defvar ccls-config-mode-directives
  '("compile_commands.json"
    "c" "cpp" "objective-c" "objective-cpp"
    "cu"
    "h" "hpp"))

(defsubst ccls-config--line-empty-p ()
  "Check if line is empty."
  (and (not (bobp))
       (save-excursion
	 (beginning-of-line)
	 (looking-at-p "[ \t]*$"))))

(defun ccls-config-mode-completion-at-point ()
  "Completion at point function."
  (let* ((pos (point))
         (end (save-excursion (skip-chars-forward "[:alnum:]_" (pos-eol)) (point)))
         (beg (unless (or (ccls-config--line-empty-p)
                          (eq (char-syntax (char-before pos)) ?\s))
                (save-excursion
                  (skip-chars-backward "[:alnum:]_" (pos-bol))
                  (point)))))
    (when (and beg end)
      (nconc (list beg end)
             (cond
              ((eq ?% (char-before beg))
               (list ccls-config-mode-directives
                     :annotation-function (lambda (_s) "<directive>")))
              ((eq ?- (char-before beg))
               (list '("I" "isystem" "D")))
              (t (list ccls-config-mode-driver
                       :annotation-function (lambda (_s) "<driver>"))))))))

(defvar ccls-config-mode-font-lock-keywords
  `((,(concat "^" (regexp-opt ccls-config-mode-driver) "\\_>")
     . font-lock-keyword-face)
    (,(concat "^%" (regexp-opt ccls-config-mode-directives) "\\_>")
     . font-lock-builtin-face)))

;;;###autoload
(define-derived-mode ccls-config-mode conf-mode "CCLS"
  "Major mode for .ccls config."
  :group 'conf
  (setq-local font-lock-defaults '(ccls-config-mode-font-lock-keywords))
  (when (featurep 'yasnippet)
    (yas-minor-mode))
  (add-hook 'completion-at-point-functions #'ccls-config-mode-completion-at-point nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ccls\\'" . ccls-config-mode))

(provide 'ccls-config-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ccls-config-mode.el ends here
