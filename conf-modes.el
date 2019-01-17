;;; conf-modes.el --- Major modes for configuration files -*- lexical-binding: t; -*-

;; Author: noah peart <noah.v.peart@gmail.com>
;; Last modified: <2019-01-16 18:38:58>
;; URL: http://github.com/nverno/conf-modes
;; Package-Requires: 
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

;; Configuration modes:
;; 
;; - `xkb-mode'       => xkb configuration files
;;
;; - `dotctags-mode'  => .ctags
;;
;; - `indentpro-mode' => .indent.pro
;;
;; - `npmrc-mode'     => .npmrc
;;
;; - `authinfo-mode'  => .authinfo[.gpg]
;;
;; - `netrc-mode'     => .netrc[.gpg]
;;
;; - `ssh-config-mode' => .sss/config | ssh_config
;;
;; - etc.
;;
;; `conf-macros' generates company backends from calling some sort of
;;     "--help"-like output from a command-line program

;;; Code:

(provide 'conf-modes)

;;; conf-modes.el ends here
