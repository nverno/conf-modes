;;; dotctags-mode --- .ctags mode

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 29 September 2016

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
  (require 'cl-lib))
(require 'company)
(require 'conf-mode)

;; ------------------------------------------------------------
;;* Mode

(defun dotctags-propertize (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("^\\s-*#.*" (0 "<")))
   (point) end))

(defvar dotctags-mode-syntax-table
  (let ((st (make-syntax-table conf-windows-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    ;; (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st))

;;;###autoload
(define-derived-mode dotctags-mode conf-windows-mode "Conf[cTags]"
  "Conf Mode for ctags config."
  :syntax-table dotctags-mode-syntax-table
  (conf-mode-initialize "#")
  (setq-local comment-end "")
  (setq-local company-backends '(company-dotctags))
  (setq-local syntax-propertize-function #'dotctags-propertize))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ctags\\'" . dotctags-mode))

;; ------------------------------------------------------------
;;* Completion

(defvar company-dotctags-keywords nil)
(defun company-dotctags-keywords ()
  "List of options for ctags."
  (or company-dotctags-keywords
      (setq company-dotctags-keywords
            (let ((lines (process-lines "ctags" "--help")))
              (cl-loop for line in lines
                 when (string-match "^\\s-*--\\([^=]+\\)=\\([^\n]*\\)" line)
                 collect (propertize
                          (concat "--" (match-string-no-properties 1 line))
                          'annot (match-string-no-properties 2 line)))))))

(defun company-dotctags-candidates (arg)
  (all-completions arg (company-dotctags-keywords)))

(defun company-dotctags-annot (arg)
  (get-text-property 0 'annot arg))

(defun company-dotctags-prefix ()
  (and (eq major-mode 'dotctags-mode)
       (not (company-in-string-or-comment))
       (company-grab-symbol)))

(defun company-dotctags (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-autoconf))
    (prefix (company-dotctags-prefix))
    (annotation (company-dotctags-annot arg))
    (candidates (company-dotctags-candidates arg))
    (duplicates nil)))

(provide 'dotctags-mode)

;;; dotctags-mode.el ends here
