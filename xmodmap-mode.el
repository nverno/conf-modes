;;; Simple X Mod Map Mode -*- lexical-binding: t; -*-
;; from emacs wiki https://www.emacswiki.org/emacs/XModMapMode

(defun xmodmap-xev ()
  (interactive)
  (let ((buff (get-buffer-create "*xrev*")))
    (pop-to-buffer buff)
    (set-process-sentinel
     (start-process "xev" buff "xev")
     #'(lambda (p _m)
         (when (zerop (process-exit-status p))
           (kill-buffer buff))))))

;;;###autoload
(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  (list
   (function
    (lambda ()
      (local-set-key (kbd "C-c C-c") 'xmodmap-xev))))
  "Simple mode for xmodmap files.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[xX]modmap\\(rc\\)?\\'" . xmodmap-mode))

(provide 'xmodmap-mode)
