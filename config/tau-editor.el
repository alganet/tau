(setq linum-format "%4d ")
(setq-default truncate-lines t)
(setq auto-save-default nil)
(setq-default cursor-type '(bar . 2))

(cua-mode t)
(linum-mode t)
(blink-cursor-mode t)
(column-number-mode t)
(ido-mode t)
(show-paren-mode t)

(global-font-lock-mode t)

(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)

(provide 'tau-editor)
