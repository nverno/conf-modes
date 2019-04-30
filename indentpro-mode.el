;;; company-indentpro.el --- Major mode for .indent.pro -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires:
;; Created:  5 August 2016
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

;;; Commentary:

;; Major mode for editing .indent.pro files.  
;; Completion support in company-indentpro.el
;;
;; It also adds font-locking and c/c++ comment syntax.

;; Installation:

;; Just add this file to `load-path' and require it or compile and
;; make autoloads, then load the autoloads files in your init file.
;; The simple way:

;; ```lisp
;; (require 'indentpro-mode)
;; ```

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (defvar company-backends))

(defgroup indentpro nil
  "Emacs major mode for .indent.pro files."
  :group 'languages)

(defcustom indentpro-use-company (featurep 'company)
  "Use company completion backend.  When true, pushes
 `company-indentpro' to local `company-backends'."
  :group 'indentpro
  :type 'boolean)

(defvar indentpro-common-styles
  '((gnu "-nbad" "-bap" "-nbc" "-bbo" "-bl" "-bli2" "-bls" "-ncdb" "-nce"
         "-cp1" "-cs" "-di2 -ndj" "-nfc1" "-nfca" "-hnl" "-i2" "-ip5"
         "-lp"
         "-pcs" "-nprs" "-psl" "-saf" "-sai" "-saw" "-nsc" "-nsob")
    (k&r "-nbad" "-bap" "-bbo" "-nbc" "-br" "-brs" "-c33" "-cd33" "-ncdb"
         "-ce" "-ci4" "-cli0" "-cp33" "-cs" "-d0" "-di1" "-nfc1" "-nfca"
         "-hnl" "-i4" "-ip0" "-l75" "-lp" "-npcs" "-nprs" "-npsl" "-saf"
         "-sai" "-saw" "-nsc" "-nsob" "-nss")
    (berkeley "-nbad" "-nbap" "-bbo" "-bc" "-br" "-brs" "-c33" "-cd33"
              "-cdb" "-ce" "-ci4" "-cli0" "-cp33" "-di16" "-fc1" "-fca"
              "-hnl" "-i4" "-ip4" "-l75" "-lp" "-npcs" "-nprs" "-psl"
              "-saf" "-sai" "-saw" "-sc" "-nsob" "-nss" "-ts8")
    (linux "-nbad" "-bap" "-nbc" "-bbo" "-hnl" "-br" "-brs" "-c33"
           "-cd33" "-ncdb" "-ce" "-ci4" "-cli0" "-d0" "-di1" "-nfc1"
           "-i8" "-ip0" "-l80" "-lp" "-npcs" "-nprs" "-npsl" "-sai"
           "-saf" "-saw" "-ncs" "-nsc" "-sob" "-nfca" "-cp33" "-ss"
           "-ts8" "-il1")))

(defvar indentpro-font-lock-keywords
  '(("^-[-A-Za-z0-9]+" . font-lock-type-face)))

;; ------------------------------------------------------------
;;; Major Mode

;; Syntax / for c/c++ style comments
(defvar indentpro-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\* ". 23" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Sytax for `indentpro-mode'.")

;;;###autoload
(define-derived-mode indentpro-mode conf-mode "Indent Pro"
  "Major mode for .indent.pro files.\n"
  (setq-local comment-start "/* ")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-end " */")
  (setq-local font-lock-defaults '(indentpro-font-lock-keywords))
  
  ;; Enable company-indentpro backend
  (when (and (featurep 'company)
             (require 'company-indentpro nil t)
             indentpro-use-company)
    (make-local-variable 'company-backends)
    (cl-pushnew 'company-indentpro company-backends)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.indent\\.pro\\'" . indentpro-mode))

(provide 'indentpro-mode)

;;; company-indentpro.el ends here
