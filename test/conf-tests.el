(require 'xkb-mode)
(require 'ert)

(defmacro conf--should-indent (mode from to)
  (declare (indent 1))
  `(with-temp-buffer
     (funcall ,mode)
     (insert ,from)
     (indent-region (point-min) (point-max))
     (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      ,to))))

(ert-deftest conf--xkb-indent ()
  "xkb indents"
  (conf--should-indent 'xkb-mode
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

(defun conf--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "conf--test")
    (message "cant run without ert.")))

(provide 'conf-tests)
