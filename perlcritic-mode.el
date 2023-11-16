;;; perlcritic-mode.el --- Perl config files -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: ((emacs "25.1"))
;; Created: 25 March 2020
;; Version: 0.1.0
;; Keywords: languages

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
;; Perl config files
;; - .perlcritic
;; - .perltidyrc
;;
;;; Code:

(require 'conf-mode)
(require 'conf-completion)

;;;###autoload
(define-derived-mode perlcritic-mode conf-toml-mode "Conf[perlrc]"
  "Major mode for perlcritic config."
  :group 'conf
  (modify-syntax-entry ?- "'")
  (conf-completion-initialize))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.perlcriticrc\\'" . perlcritic-mode))

(provide 'perlcritic-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; perlcritic-mode.el ends here
