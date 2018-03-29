(defvar tau/esc-quit-delay 0.01)
(defvar tau/esc-quit-minibuffer-delay 0.1)
(defvar tau/esc-quit-wait-delay 0.2)
(defvar tau/escaping nil)

(transient-mark-mode 1)
(delete-selection-mode 1)
(column-number-mode 1)
(fringe-mode '(0 . 0))

(defvar my-linum-format-string " %4d")
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(defun my-linum-get-format-string ()

(let* ((width (length (number-to-string
                      (count-lines (point-min) (point-max)))))
      (format (concat " %" (number-to-string (min 2 width)) "d")))
 (setq my-linum-format-string format)))

(defun my-linum-format (line-number)
	(concat
		(propertize (format my-linum-format-string line-number) 'face 'linum)
		(propertize "\u2502 " 'face 'fringe)))

(setq linum-format 'my-linum-format)
(global-linum-mode t)

(set-default 'truncate-lines t)

(provide 'tau-editor)