;;; ssh-config-mode.el --- Major mode for SSH configs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/conf-modes
;; Package-Requires: 
;; Created: 14 December 2018
;; Version: 0.1.0
;; Keywords:

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
;; Major mode for SSH config files: font-lock, syntax, indentation, help
;;; Code:

(require 'smie)

(defcustom ssh-config-indent-offset 5
  "Number of spaces for indentation."
  :group 'conf
  :type 'integer)

(defvar ssh-config-keywords
  (eval-when-compile
    `((,(regexp-opt
         '("AddKeysToAgent" "AddressFamily" "BadOption" "BatchMode" "BindAddress"
           "BindInterface" "CASignatureAlgorithms" "CanonicalDomains"
           "CanonicalizeFallbackLocal" "CanonicalizeHostname" "CanonicalizeMaxDots"
           "CanonicalizePermittedCNAMEs" "CertificateFile"
           "ChallengeResponseAuthentication" "CheckHostIP" "Cipher" "Ciphers"
           "ClearAllForwardings" "Compression" "CompressionLevel" "ConnectTimeout"
           "ConnectionAttempts" "ControlMaster" "ControlPath" "ControlPersist"
           "Deprecated" "DynamicForward" "EnableSSHKeysign" "EscapeChar"
           "ExitOnForwardFailure" "FingerprintHash" "ForwardAgent" "ForwardX"
           "GatewayPorts" "GlobalKnownHostsFile" "GssAuthentication"
           "GssDelegateCreds" "HashKnownHosts" "Host" "HostKeyAlgorithms"
           "HostKeyAlias" "HostName" "HostbasedAuthentication" "HostbasedKeyTypes"
           "IPQoS" "IdentitiesOnly" "IdentityAgent" "IdentityFile" "Ignore"
           "IgnoreUnknown" "IgnoredUnknownOption" "Include"
           "KbdInteractiveAuthentication" "KbdInteractiveDevices" "KexAlgorithms"
           "LocalCommand" "LocalForward" "LogFacility" "LogLevel" "Macs" "Match"
           "NoHostAuthenticationForLocalhost" "NumberOfPasswordPrompts" "PKCS"
           "PasswordAuthentication" "PermitLocalCommand" "Port"
           "PreferredAuthentications" "ProxyCommand" "ProxyJump" "ProxyUseFdpass"
           "PubkeyAcceptedKeyTypes" "PubkeyAuthentication" "RSAAuthentication"
           "RekeyLimit" "RemoteCommand" "RemoteForward" "RequestTTY"
           "RevokedHostKeys" "RhostsRSAAuthentication" "SendEnv"
           "ServerAliveCountMax" "ServerAliveInterval" "SetEnv"
           "StreamLocalBindMask" "StreamLocalBindUnlink" "StrictHostKeyChecking"
           "TCPKeepAlive" "Tunnel" "TunnelDevice" "Unsupported" "UpdateHostkeys"
           "UsePrivilegedPort" "User" "UserKnownHostsFile" "VerifyHostKeyDNS"
           "VisualHostKey" "XAuthLocation"
           ;; openssh-clien additions
           "SendEnv" "HashKnownHosts" "GSSAPIAuthentication")
         'paren)
       (1 font-lock-keyword-face)))))

(defconst ssh-config-grammar
  (smie-prec2->grammar
   (smie-precs->prec2
    '((assoc "Host" "host" "") (assoc "\n")))))

(defun ssh-config-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic))
    (`(:elem . args) (smie-indent-keyword "\n"))
    (`(:before . "\n")
     (- ssh-config-indent-offset (current-column)))
    (`(:list-intro . ,(or `"" `"Host" `"host")) 0)))

(declare-function Man-goto-section "man")
(defun ssh-config-help (&optional key)
  "Show man page for ssh_config.
Search for KEY or symbol at point if possible."
  (interactive)
  (setq key (or key (thing-at-point 'symbol)))
  (man "ssh_config")
  (sit-for 0.1)                         ;man doesn't return buffer or process
  (pop-to-buffer "*Man ssh_config*")
  (Man-goto-section "DESCRIPTION")
  (when key
    (let ((re (concat "^\\s-*" (regexp-quote key))))
      (catch 'done
        (while (re-search-forward re)
          (and (equal '(face Man-overstrike) (text-properties-at (1- (point))))
               (throw 'done t)))))))

(defvar ssh-config-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "M-?") #'ssh-config-help)
    km)
  "Keymap for `ssh-config-mode'.")

(defvar ssh-config-mode-syntax-table
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?# "<" tab)
    (modify-syntax-entry ?\n ">" tab)
    (modify-syntax-entry ?\. "_" tab)
    tab)
  "Syntax table for `ssh-config-mode'.")

;;;###autoload
(define-derived-mode ssh-config-mode prog-mode "SSH"
  "Major mode for ssh configs."
  :group 'conf
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local imenu-generic-expression '((nil "\\s-*[Hh]ost\\s-+\\(.*\\)\\s-*$" 1)))
  (smie-setup ssh-config-grammar #'ssh-config-smie-rules)
  (setq-local font-lock-defaults '(ssh-config-keywords nil 'case-fold)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.?sshd?[_/]config\\'" . ssh-config-mode))

(provide 'ssh-config-mode)
;;; ssh-config-mode.el ends here
