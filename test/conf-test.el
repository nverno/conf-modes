(require 'ert)
(require 'xkb-mode)
(require 'indentpro-mode)
(require 'company-indentpro)

(defun conf-test-set-point ()
  "Leave point at first '|'."
  (let (end-char)
    (when (search-forward "|" nil 'move)
      (delete-char -1)
      (setq end-char ?|))
    end-char))

(defmacro conf--should-become (mode from to &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (funcall ,mode)
     (save-excursion (insert ,from))
     (let ((end-char (conf-test-set-point)))
       ,@body
       (and end-char (insert end-char)))
     (indent-region (point-min) (point-max))
     (should (string= (buffer-string) ,to))))

(ert-deftest conf--xkb-indent ()
  "xkb indents"
  :tags '(:xkb)
  (conf--should-become 'xkb-mode
    "
default
xkb_keycodes \"qwerty\" {

alias <LatQ> = <AD01>;
}"
    "
default
xkb_keycodes \"qwerty\" {

    alias <LatQ> = <AD01>;
}"))

(ert-deftest conf-indent-comp ()
  ".indent.pro completion"
  :tags '(:indentpro)
  (skip-unless (executable-find "indent"))
  (conf--should-become 'indentpro-mode
    "--brace-|"
    "--brace-indentn|"
    (company-mode)
    (let ((company-backends '(company-indentpro)))
      (company-complete))))

(provide 'conf-test)
