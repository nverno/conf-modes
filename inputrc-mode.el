;;; inputrc-mode.el --- Major mode for readline(3) configuration -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Created: 24 November 2023
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
;; Major mode for readline(3) configuration - inputrc files.
;;
;; Features:
;; - font-locking
;; - indentation
;;
;;; Code:

(require 'conf-mode)
(require 'smie)

(defcustom inputrc-mode-indent-level 2
  "Number of spaces for each indentation step."
  :group 'inputrc
  :type 'integer
  :safe 'integerp)

(defconst inputrc-mode-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((exp)
      (cmd ("$if" exp "$endif")
           ("$if" exp "$else" exp "$endif"))))))

(defun inputrc-mode-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) inputrc-mode-indent-level)
    (`(:elem . args))
    (`(:after . ,(or "$if" "$else")) inputrc-mode-indent-level)
    (`(:list-intro . ,(or "" "\n" "$if")) t)
    (`(:close-all . ,_) t)))

(defvar inputrc-mode-font-lock-keywords
  `(("\\($include\\)\\s-+\\([[:graph:]]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-string-face))
    (,(rx "$" (or "if" "else" "endif")) . font-lock-keyword-face)
    (,(rx symbol-start (or "on" "off") symbol-end) . font-lock-constant-face)
    ("\\b[0-9]+\\b" . 'font-lock-number-face)
    ("^\\s-*\\(set\\)\\s-+\\([[:graph:]]+\\)\\s-+\\([[:graph:]]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-string-face))
    ((lambda (lim)
       (and (re-search-forward "\\(\\\\[A-Za-z]\\(?:-[^\\]?\\)?\\)" lim t)
            (null (nth 4 (syntax-ppss (match-beginning 0))))))
     (1 'font-lock-type-face t))
    ("\".*\"[ \t]*:[ \t]*\\([[:graph:]]+\\)" (1 font-lock-variable-name-face))
    ("\\([^ \t\"]+\\)[ \t]*:[ \t]*\\([[:graph:]]+\\)"
     (1 font-lock-type-face)
     (2 font-lock-variable-name-face))))

(defun inputrc-mode--syntax-propertize (start end)
  "Apply syntax properties to text between START and END."
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("\\([[:graph:]]+\\)[ \t]*:[ \t]*\\([[:graph:]]+\\)[ \t]+\\([^ \t\n]\\)"
     (3 "<")))
   (point) end))

;;;###autoload
(define-derived-mode inputrc-mode conf-unix-mode "Conf[inputrc]"
  "Major mode for readline(3) configuration."
  :group 'conf
  (conf-mode-initialize "#")
  (setq-local font-lock-defaults '(inputrc-mode-font-lock-keywords))
  (smie-setup inputrc-mode-grammar #'inputrc-mode-smie-rules)
  (setq-local syntax-propertize-function #'inputrc-mode--syntax-propertize))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.?inputrc\\'" . inputrc-mode))

(provide 'inputrc-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; inputrc-mode.el ends here
