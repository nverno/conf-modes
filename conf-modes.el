;;; conf-modes.el --- Major modes for editing various configuration files

;; Author: noah peart <noah.v.peart@gmail.com>
;; URL: http://github.com/nverno/conf-modes

;; This file is not part of GNU Emacs

;; This is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Configuration modes:
;; 
;; - `xkb-mode' for xkb configuration files
;;
;; - `w32-registry-mode' for windows registry files
;;  Adds some convenience functions to look at registry key values, basic
;;  menu, bindings, imenu, etc.
;;  Completion for registry values
;;  [company-w32reg](http://github.com/nverno/company-w32reg)
;;
;; - `dotctags-mode' for .ctags files
;;
;; `conf-macros' generates company backend from --help output.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'pp))

(defgroup w32-registry nil
  "Simple mode for windows registry files."
  :group 'languages
  :prefix "w32-registry-")

;; ------------------------------------------------------------
;;* variables
(defcustom w32-registry-header
  "Windows Registry Editor Version 5.00"
  "Header to insert in '.reg' file."
  :group 'w32-registry
  :type 'string)

(defcustom w32-registry-exe
  (or (executable-find "reg.exe")
      (expand-file-name "system32/reg.exe" (getenv "windir")))
  "Location of 'reg.exe' executable."
  :group 'w32-registry
  :type 'string)

(defcustom w32-registry-regedit-exe
  (or (executable-find "regedit.exe")
      (expand-file-name "regedit.exe" (getenv "windir")))
  "Location of regedit.exe executable."
  :group 'w32-registry
  :type 'string)

(defcustom w32-registry-regex
  "\\([A-Za-z_0-9]+\\)\\s-*\\(REG_[A-Za-z]+\\)\\s-*\\([0-9a-zA-Z]+\\)"
  "Regex to match registry entries: (subkey, type, value)."
  :group 'w32-registry
  :type 'regex)

(defcustom w32-registry-auto-header t
  "If non-nil, automatically insert `w32-registry-template' in empty
buffers."
  :group 'w32-registry
  :type 'boolean)


;; ------------------------------------------------------------
;;* mode
(defvar Reg-menu
  '("Registry"
    ["Lookup" w32-registry-template :help "Show registry key in buffer" :keys "C-c C-?"]
    ["List" w32-registry-list :help "Convert registry key contents to list" :keys "C-c C-l"]
    ["Value" w32-registry-value :help "Show value of subkey" :keys "C-c C-v"]
    "--"
    ["Regedit" w32-registry-regedit :help "Open regedit.exe" :keys "C-c C-e"]
    "--"
    ["Template" w32-registry-template :help "Insert template" :keys "C-c C-t"]))

(defvar w32-registry-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil Reg-menu)
    (define-key map (kbd "C-c C-t") #'w32-registry-template)
    (define-key map (kbd "C-c C-?") #'w32-registry-lookup)
    (define-key map (kbd "C-c C-l") #'w32-registry-list)
    (define-key map (kbd "C-c C-v") #'w32-registry-value)
    (define-key map (kbd "C-c C-e") #'w32-registry-regedit)
    map))

(define-abbrev-table 'w32-registry-mode-abbrev-table
  '(("hkcu" "HKEY_CURRENT_USER" nil :system t)
    ("hklm" "HKEY_LOCAL_MACHINE" nil :system t))
  "w32-registry abbrevs table."
  :case-fixed nil)

;;;###autoload
(define-derived-mode w32-registry-mode conf-windows-mode "Reg"
  "Major mode for editing windows registry files.\n
\\{w32-registry-mode-map}"
  (setq-local local-abbrev-table w32-registry-mode-abbrev-table)
  (setq-local imenu-generic-expression '((nil "^\\[\\([^\]]+\\)\\].*" 1)))
  (setq-local imenu-case-fold-search t)
  (setq-local case-fold-search t)
  (setq-local outline-regexp "[^\]]")
  (when (and w32-registry-auto-header
             (= 0 (buffer-size (current-buffer))))
    (w32-registry-template)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.reg$" . w32-registry-mode))


;; ------------------------------------------------------------
;;* functions
(defun w32-registry-regedit ()
  "Open regedit.exe externally."
  (interactive)
  (call-process-shell-command w32-registry-regedit-exe nil 0))

(defun w32-registry-lookup (key)
  "Show value of registry KEY in buffer."
  (interactive "sRegKey: ")
  (let ((buff (get-buffer-create "*w32-registry*")))
    (with-output-to-temp-buffer buff
      (call-process w32-registry-exe nil buff t "query" key))))

(defun w32-registry-list (key)
  "Lookup KEY in registry, return list of form ((subkey type value)) or nil if 
no entries/error."
  (interactive "sRegKey: ")
  (let (res)
    (with-temp-buffer
      (call-process w32-registry-exe nil t nil "query" key)
      (goto-char (point-min))
      (while
          (re-search-forward w32-registry-regex nil t)
        (push `(,(match-string-no-properties 1)
                ,(match-string-no-properties 2)
                ,(match-string-no-properties 3))
              res))
      res)))

(defun w32-registry-value (key subkey)
  "Get SUBKEY from KEY in the registry.  Returns list of form (subkey type value)."
  (interactive "sRegKey: \nsSubkey: ")
  (let* ((subkeys (w32-registry-list key))
         (res (cl-find subkey subkeys
                       :test (lambda (x y) (string= (downcase x) (downcase (car y)))))))
    (when (called-interactively-p 'any)
      (pp res))
    res))

(defun w32-registry-template ()
  "Insert header for '.reg' file.  If buffer is empty, template is 
inserted and point moved to end of buffer (ie. if in `conf-windows-mode-hook'),
otherwise template is inserted at beginning of buffer and point preserved."
  (interactive)
  (let ((size (buffer-size (current-buffer))))
    (save-excursion
      (goto-char (point-min))
      (insert w32-registry-header)
      (insert "\n"))
    (when (= 0 size) (goto-char (point-max)))))

(provide 'w32-registry-mode)

;;; conf-modes.el ends here
