;;; conf-modes.el --- Major modes for configuration files -*- lexical-binding: t; -*-

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
