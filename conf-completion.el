;;; conf-completion.el --- completion for config files -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: 
;; Created:  9 March 2020

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
;; Provide completion-at-point (which can be used by `company-capf') for
;; config files, given a command to produce a list of options and a regex
;; to extract the candidates.
;;
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;; Variables to setup in mode hooks

(defvar conf-completion-program nil
  "Program to call to get completion candidates.")

(defvar conf-completion-args '("--help")
  "Arguments passed to `conf-completion-program' to generate the candidates.")

(defvar conf-completion-regexp
  '("^\\s-*\\(--?[^=]+\\)=\\([^\n]*\\)"
    1                                   ; capture group for candidate
    2                                   ; capture group for annotation
    )
  "Regexp to match results from help command.
Match groups are interpreted according to positions following the regexp.
The first group is the candidate, the second any associated annotation.
If no group numbers are given, the whole match is assumed to be the candidate.")

(defvar conf-completion-verbose t "When non-nil output messages.")

(defvar conf-completion-process-filter nil "Process filter to use if non-nil")

;; internal
(defvar conf-completion-cache (make-hash-table :test #'equal)
  "Cache completion candidates for programs.")

;; -------------------------------------------------------------------
;;; Process

(defun conf-completion-msg (format &rest args)
  (and conf-completion-verbose (apply #'message format args)))

;; parse candidates in process output buffer
;; note: pass the regexp since it will be set buffer-locally in a mode hook
(defun conf-completion-parse-output (conf-regexp)
  (goto-char (point-min))
  (cl-destructuring-bind (regexp &optional cand annot) conf-regexp
    (let ((cand-pos (or cand 0)) ; match position of candidate
          res)
      (while (re-search-forward regexp nil t)
        (push (propertize (match-string cand-pos) 'annotation (match-string annot))
              res))
      res)))

(defun conf-completion-process-sentinel (conf-regexp)
  "Process sentinel used by `conf-completion-start-process-async'.
Parses candidates, then re-enables the completion-at-point function."
  `(lambda (proc event)
     (if (not (zerop (process-exit-status proc)))
         (progn
           (conf-completion-msg "%s failed: %S" (process-name proc) event)
           (kill-buffer (process-buffer proc)))
       (let ((pbuf (process-buffer proc))
             (pname (process-name proc))
             res
             enable-completion)
         (unwind-protect
             (when (buffer-live-p pbuf)
               (with-current-buffer pbuf
                 (setq res (conf-completion-parse-output ',conf-regexp))
                 (when res
                   (puthash pname res conf-completion-cache)
                   (setq enable-completion t))))
           (kill-buffer pbuf))
         (if (not enable-completion)
             (progn
               (puthash pname 'fail conf-completion-cache)
               (conf-completion-msg "%s failed to find candidates" pname))
           (add-hook 'completion-at-point-functions
                     #'conf-completion-at-point nil t)
           (conf-completion-msg "Completion enabled for %s" pname))))))

(defun conf-completion-start-process-async ()
  "Call `conf-completion-program' and parse results to get completion candidates."
  (conf-completion-msg
   "Running %s %s" conf-completion-program
   (mapconcat 'identity conf-completion-args " "))
  (let* ((pbuf (generate-new-buffer-name
                (concat "*" conf-completion-program "*")))
         (proc (apply #'start-process conf-completion-program
                      pbuf conf-completion-program conf-completion-args)))
    (when conf-completion-process-filter
      (set-process-filter proc conf-completion-process-filter))
    (set-process-sentinel
     proc (conf-completion-process-sentinel conf-completion-regexp))
    proc))

;; -------------------------------------------------------------------
;;; Completion

;;;###autoload
(defun conf-completion-at-point ()
  "Completion at point for configs.
Must be configured with to work for a given program with a way to retrieve
the possible configuration candidates, see `conf-completion-program',
`conf-completion-args', `conf-completion-regexp'."
  (when-let ((bnds (bounds-of-thing-at-point 'symbol)))
    (let ((candidates (gethash conf-completion-program conf-completion-cache)))
      (cond
       ((null conf-completion-program)
        (remove-hook 'completion-at-point-functions #'conf-completion-at-point t)
        (user-error "Must setup `conf-completion-program'"))
       ((or (null candidates) (eq 'fail candidates))
        (remove-hook 'completion-at-point-functions #'conf-completion-at-point t)
        (when (null candidates)
          (conf-completion-start-process-async)
          nil))
       (t
        (list (car bnds) (cdr bnds) candidates
              :annotation-function
              (lambda (s)
                (concat " " (or (get-text-property 0 'annotation s) "")))))))))

(provide 'conf-completion)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; conf-completion.el ends here
