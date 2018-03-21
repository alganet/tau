(setq linum-format "%4d ")
(setq-default truncate-lines t)
(setq auto-save-default nil)
(setq-default cursor-type 'box)

(cua-selection-mode t)
(column-number-mode t)
(ido-mode t)
(show-paren-mode t)

(global-font-lock-mode t)

(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(linum-mode t)

(provide 'tau-editor)
