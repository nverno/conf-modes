;;; conf-macros --- generate company completion backends from docs -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created:  1 October 2016

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

;;; Description:
;;; Code:
(require 'cl-lib)

(cl-defmacro company-conf (name
                           &key
                           program 
                           (keyword-re "^\\s-*\\(--?[^=]+\\)=\\([^\n]*\\)")
                           (keyword-re-pos
                            '((candidate . 1)
                              (annotation . 2))))
  "Create company backend for mode NAME calling PROGRAM --help and parsing with
KEYWORD-RE to get options.  KEYWORD-RE-POS specifies capture groups for 
completions candidate and its annotation."
  (let* ((comp (concat "company-" (symbol-name name)))
         (keywords (intern (concat comp "-keywords")))
         (prefix (intern (concat comp  "-prefix")))
         (candidates (intern (concat comp "-candidates")))
         (annotation (intern (concat comp "-annotation")))
         (complete (intern comp)))
    `(progn
       (defvar ,keywords nil)
       (defun ,keywords nil
         (or ,keywords
             (setq ,keywords
                   (let ((lines (process-lines ,program "--help")))
                     (cl-loop for line in lines
                        when (string-match ,keyword-re line)
                        collect (propertize
                                 (match-string-no-properties
                                  ,(cdr (assoc 'candidate keyword-re-pos)) line)
                                 'annotation
                                 (match-string-no-properties
                                  ,(cdr (assoc 'annotation keyword-re-pos)) line)))))))

       (defun ,prefix nil
         (and (eq major-mode ',(intern (concat (symbol-name name) "-mode")))
              (not (company-in-string-or-comment))
              (company-grab-symbol)))

       (defun ,annotation (arg)
         (or (get-text-property 0 'annotation arg) ""))

       (defun ,candidates (arg)
         (all-completions arg (,keywords)))

       (defun ,complete (command &optional arg &rest _args)
         (interactive (list 'interactive))
         (cl-case command
           (prefix (,prefix))
           (annotation (,annotation arg))
           (candidates (,candidates arg))
           (duplicates nil))))))


;; (company-conf dotctags :program "ctags")

(provide 'conf-macros)

;;; conf-macros.el ends here
