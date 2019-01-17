;;; company-indentpro.el --- Company backend .indent.pro files. -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Created:  6 August 2016
;; Last modified: <2019-01-16 18:37:41>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: ((company))
;; Keywords: languages tools matching

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
;;; History:
;;; Commentary:

;; Completion support (`company-mode') backend for .indent.pro files.

;; Provides:

;; + meta information for command options
;; + annotation showing long options for short options and vice versa
;; + support for `company-doc' (ie ~C-h~ in company completion).

;;; Installation:

;; See [indentpro-mode](https://github.com/nverno/indentpro-mode) for
;; simple accompanying major mode.

;; Install `company-mode' and add this file to `load-path'.
;; Then either compile/create autoloads and load autoloads files,
;; or require the file in your init file:

;; ```lisp
;; (require 'company-indentpro-mode) ; or autoload

;; ;; Either set `indentpro-use-company' or add a custom hook,
;; ;; For example:
;; 
;; (add-hook 'indentpro-mode-hook
;;           #'(lambda ()
;;               (set (make-local-variable 'company-backends)
;;                    '((company-indentpro company-dabbrev-code)
;;                       company-dabbrev))))
;; ```

;;; Example:

;; ![test](test.png)

;;; Code:
(require 'company)

(defgroup company-indentpro nil
  "Company completion backend for '.indent.pro' files."
  :group 'company
  :group 'indentpro)

(defcustom company-indentpro-start-regexp
  "^OPTIONS"
  "Regex determining where to start parsing options."
  :group 'company-indentpro
  :type 'regex)

(defcustom company-indentpro-end-regexp
  "^INVOKING"
  "Regex determining where to stop parsing options."
  :group 'company-indentpro
  :type 'regex)

;; ------------------------------------------------------------
;;* Internal
(defvar company-indentpro-modes '(indentpro-mode)
  "Modes to activate `company-indentpro'.")

(defvar company-indentpro-candidates-list nil
  "Cache completion candidates.")

(defvar company-indentpro-regexp
  (eval-when-compile
   (concat
    "\\s-*\\(-[a-zA-Z]+\\),?" ; short option
    "\\s-*\\(-[-A-Za-z]+\\)?" ; long option
    "\n?\\s-*\\([^\n]+\\)"    ; desciption
    ))
  "Regex to capture long and short options from `man indent' 
output.")


;; ------------------------------------------------------------
;;* Parse output

(defun company-indentpro-build-list ()
  "Build candidate list."
  (let (res short long end)
    (with-temp-buffer
      (call-process "man" nil t nil "indent")
      (goto-char (point-min))
      (re-search-forward company-indentpro-start-regexp)
      (save-excursion
        (setq end (re-search-forward company-indentpro-end-regexp)))
      (forward-line 1)
      (while (not (or (eobp)
                      (< end (point))))
        (when (re-search-forward company-indentpro-regexp end t)
          (setq short (match-string-no-properties 1))
          (put-text-property
           0 1 'meta (match-string-no-properties 3) short)
          (setq long (match-string-no-properties 2))
          (when long
            (put-text-property
             0 1 'annot (match-string-no-properties 1) long)
            (put-text-property
             0 1 'annot (match-string-no-properties 2) short)
            (put-text-property
             0 1 'meta (match-string-no-properties 3) long)
            (push long res))
          (push short res))
        (forward-line 1)
        (goto-char (line-beginning-position))))
    (setq company-indentpro-candidates-list (sort res 'string<))
    company-indentpro-candidates-list))

(defun company-indentpro-prefix ()
  (and (derived-mode-p major-mode company-indentpro-modes)
       (not (company-in-string-or-comment))
       (company-grab-symbol)))

(defun company-indentpro-meta (candidate)
  (get-text-property 0 'meta candidate))

(defun company-indentpro-doc (candidate)
  (with-temp-buffer
    (call-process "man" nil t nil "indent")
    (goto-char (point-min))
    (search-forward
     (company-indentpro-strip-props candidate))
    (company-doc-buffer
     (buffer-substring-no-properties (line-beginning-position)
                                     (point-max)))))

(defun company-indentpro-annotation (candidate)
  (get-text-property 0 'annot candidate))

(defun company-indentpro-candidates (arg)
  (all-completions arg (or company-indentpro-candidates-list
                           (company-indentpro-build-list))))

(defun company-indentpro-strip-props (arg)
  (substring-no-properties arg))

;;;###autoload
(defun company-indentpro (command &optional arg &rest _args)
  "Indent pro backend for `company-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-indentpro))
    (prefix (company-indentpro-prefix))
    (annotation (company-indentpro-annotation arg))
    (meta (company-indentpro-meta arg))
    (doc-buffer (company-indentpro-doc arg))
    (sorted t)
    (candidates (company-indentpro-candidates arg))))

(provide 'company-indentpro)
;;; company-indentpro.el ends here
