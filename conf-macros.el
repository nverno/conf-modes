;;; conf-macros.el --- Generate company completion backends from docs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Created:  1 October 2016
;; Last modified: <2019-01-16 18:38:01>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: 

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

;; Generates company backends for conf-modes from program help output
;; that lists available options

;;; Code:
(require 'cl-lib)

(cl-defmacro company-conf (name
                           &key
                           program            ;call this program to produce help
                           (args '("--help")) ;program args
                           (keyword-re "^\\s-*\\(--?[^=]+\\)=\\([^\n]*\\)")
                           (keyword-re-pos
                            '((candidate . 1)
                              (annotation . 2))))
  "Create company backend for mode NAME calling PROGRAM --help and parsing with
KEYWORD-RE to get options.  KEYWORD-RE-POS specifies capture groups for 
completions candidate and its annotation."
  (declare (indent defun) (debug t))
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
                   (let ((lines (process-lines ,program ,@args)))
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

(provide 'conf-macros)
;;; conf-macros.el ends here
