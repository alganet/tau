(defvar tau/esc-quit-delay 0.01)
(defvar tau/esc-quit-minibuffer-delay 0.1)
(defvar tau/esc-quit-wait-delay 0.2)
(defvar tau/escaping nil)

(transient-mark-mode 1)
(delete-selection-mode 1)
(column-number-mode 1)
(fringe-mode '(0 . 0))

(setq linum-format " %3d ")
(global-linum-mode t)

(add-hook 'eshell-mode-hook (lambda () 
	(linum-mode -1)))

(set-default 'truncate-lines t)

(provide 'tau-editor)
