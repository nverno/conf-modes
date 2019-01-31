;;; xmodmap-mode.el --- Xmodmap major mode -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-mode
;; Last modified: <2019-01-31 00:20:02>
;; Package-Requires: 
;; Created:  7 November 2016

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

;; Major mode for .Xmodmap config
;; See emacs wiki https://www.emacswiki.org/emacs/XModMapMode

;;; Code:

;;;###autoload
(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  "Simple mode for xmodmap files.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[xX]modmap\\(rc\\)?\\'" . xmodmap-mode))

(provide 'xmodmap-mode)
;;; xmodmap-mode.el ends here
