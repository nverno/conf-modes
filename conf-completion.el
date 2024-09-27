;;; conf-completion.el --- Completion for config files -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: ((emacs "25.1"))
;; Created:  9 March 2020
;; Version: 0.1.0
;; Keywords: convenience, matching

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
  "Program to call to get completion candidates using `start-process'.")

(defvar conf-completion-args '("--help")
  "Arguments passed to `conf-completion-program' to generate the candidates.")

(defvar conf-completion-shell-command nil
  "Command to call using `start-process-shell-command' instead of `start-process'.")

(defvar-local conf-completion-skip-leading-regexp nil
  "Regexp to match characters to skip at beginning of completion prefix.")

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

(defvar conf-completion-process-filter nil "Process filter to use if non-nil.")

(defvar conf-completion-capf-function nil
  "When non-nil, this function is applied to START END CANDIDATES and should return
list for `completion-at-point'.")

(defvar conf-completion-botap nil
  "When non-nil, this function should return a cons of \\='(start . end) for the
symbol to complete at point.")

(defvar conf-completion-alist
  `((".npmrc"
     ((program "npm" "config" "ls" "-l")
      (regexp "^\\([^; \t]+\\)\\s-*=\\s-*\\(.*\\)" 1 2)))
    (".yarnrc"
     ((program "yarn")
      (shell-command
       "yarn config --no-default-rc --json list | head -n2 | tail -n1 |
jq -Mr '.data|to_entries|map(\"\\(.key) \\(.value|tostring)\")[]'")
      (regexp "^\\([^ ]+\\) \\([^\n]+\\)\n" 1 2)))
    (".perlcriticrc"
     ((program "perlcritic" "--list")
      (regexp "^\\([[:digit:]]+\\)\\s-*\\([[:alpha:]:]+\\)" 2 1)))
    ("ctags"
     ((program "ctags" "--help")
      (regexp "^\\s-*\\(--?[^=]+\\)=\\([^\n]*\\)" 1 2)))
    ("clang-tidy"
     ((program "clang-tidy")
      (shell-command "clang-tidy --list-checks --checks=* | tail -n +2")
      (regexp "\\s-*\\(\\S-+\\)" 1)))
    (".gitconfig"
     ((program "git")
      (shell-command "git help --config | head -n -1")
      (regexp "^\\(\\S-+\\)" 1)
      (capf-function . conf-completion-gitconfig-capf))))
  "Alist mapping files to associated programs and arguments.
The output from calling these commands will be parsed to produce completion
candidates.")

;; internal
(defvar conf-completion-cache (make-hash-table :test #'equal)
  "Cache completion candidates for programs.")

(defvar-local conf-completion--callback nil)

;;;###autoload
(defun conf-completion-initialize (&optional filename callback)
  "Initialize completion variables from values in `conf-completion-alist'.
If a match is found, `conf-completion-at-point' is initialized in the buffer.
If FILENAME is non-nil, use it to match entry in `conf-completion-alist'.
If CALLBACK is given, call CALLBACK with completion candidates when they are
found."
  (interactive)
  (let ((fname (or filename
                   (file-name-nondirectory (or (buffer-file-name) (buffer-name))))))
    (when-let ((vals (car (assoc-default fname conf-completion-alist))))
      (pcase-dolist (`(,k . ,v) vals)
        (if (eq k 'program)
            (setq conf-completion-program (car v)
                  conf-completion-args (cdr v))
          (setq k (intern (concat "conf-completion-" (symbol-name k))))
          (set (make-local-variable k) v)))
      (setq-local conf-completion--callback callback)
      (add-hook 'completion-at-point-functions #'conf-completion-at-point nil t))))

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
        (push (propertize (match-string cand-pos)
                          'annotation (and annot (match-string annot)))
              res))
      res)))

(defun conf-completion-process-sentinel (conf-regexp &optional callback)
  "Process sentinel used by `conf-completion-start-process-async'.
Parses candidates using CONF-REGEXP, then re-enables the
`completion-at-point' function.
CALLBACK, if given, will be called with completion candidates."
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
           ,@(when callback `((funcall #',callback res)))
           (conf-completion-msg "Completion enabled for %s" pname))))))

(defun conf-completion-start-process-async ()
  "Call `conf-completion-program' and parse results to get completion candidates."
  (conf-completion-msg
   "Running %s %s" conf-completion-program
   (mapconcat 'identity (or conf-completion-shell-command conf-completion-args) " "))
  (let* ((pbuf (generate-new-buffer-name
                (concat "*" conf-completion-program "*")))
         (proc
          (if conf-completion-shell-command
              (start-process-shell-command
               conf-completion-program pbuf (car conf-completion-shell-command))
            (apply #'start-process conf-completion-program
                   pbuf conf-completion-program conf-completion-args))))
    (when conf-completion-process-filter
      (set-process-filter proc conf-completion-process-filter))
    (set-process-sentinel
     proc (conf-completion-process-sentinel
           conf-completion-regexp conf-completion--callback))
    proc))

;; -------------------------------------------------------------------
;;; Completion

(defun conf-completion--botap ()
  "Return bounds of completion prefix at point."
  (if conf-completion-botap
      (funcall conf-completion-botap)
    (or (bounds-of-thing-at-point 'symbol)
        (cons (point) (point)))))

;;;###autoload
(defun conf-completion-at-point ()
  "Completion at point for configs.
Must be configured with to work for a given program with a way to retrieve
the possible configuration candidates, see `conf-completion-program',
`conf-completion-args', `conf-completion-regexp'."
  (pcase-let ((`(,start . ,end) (conf-completion--botap)))
    (when start
      (when conf-completion-skip-leading-regexp
        (save-excursion
          (goto-char start)
          (while (and (< (point) end)
                      (looking-at conf-completion-skip-leading-regexp))
            ;; (forward-char 1)
            (goto-char (match-end 0)))
          (setq start (point))))
      (when (and start end (<= start end))
        (let ((candidates (gethash conf-completion-program conf-completion-cache)))
          (cond
           ((null conf-completion-program)
            (remove-hook 'completion-at-point-functions #'conf-completion-at-point t)
            (user-error "Must setup `conf-completion-program'"))
           ((or (null candidates)
                (eq 'fail candidates))
            (remove-hook 'completion-at-point-functions #'conf-completion-at-point t)
            (when (null candidates)
              (conf-completion-start-process-async)
              nil))
           (conf-completion-capf-function
            (funcall conf-completion-capf-function start end candidates))
           (t (list start end candidates
                    :annotation-function (lambda (s)
                                           (concat " " (or (get-text-property
                                                            0 'annotation s)
                                                           "")))))))))))


;;; Gitconfig Completion

(defun conf-completion-gitconfig-capf (start end candidates)
  "Completion at point for gitconfig.

Provides completion for gitconfig options (\"git help --config\") relevant to
whatever section the point is currently in."
  (let* ((section (save-excursion
                    (goto-char start)
                    (ignore-errors (beginning-of-defun))
                    (and (looking-at "\\[\\([^\] \t\n]+\\)")
                         (match-string 1))))
         (n (length section)))
    (when section
      (list start end
            (cl-loop for c in candidates
                     when (string-prefix-p section c)
                     collect (substring c (1+ n)))))))

;;; TODO(09/26/24): when point is on blank, fully indented line in
;;; `gitconfig-mode', `gitconfig-indent-line' should return `noindent' so
;;; completion-at-point happens
(defun gitconfig@indent-or-complete (_orig)
  "Advice around `gitconfig-indent-line' to return \\='noindent on an empty line.")

(provide 'conf-completion)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; conf-completion.el ends here
