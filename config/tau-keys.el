

(define-key tau-map "\C-k" nil)
(define-key tau-map [tab] 'tau/tab)
(define-key tau-map "\t" 'tau/tab)
(define-key tau-map (kbd "<backtab>") 'tau/backtab)
(define-key tau-map (kbd "<S-iso-lefftab>") 'tau/backtab)
(define-key tau-map (kbd "C-k C-p") 'counsel-M-x)

;; All ESC prefix inside C-k e combo
(define-key tau-map "\C-k\ e" nil)
(define-key tau-map "\C-k\ ep" 'counsel-M-x)
(define-key tau-map (kbd "C-k e <escape>") 'keyboard-quit)

; Universal keys
(define-key tau-map (kbd "<C-tab>") 'tabbar-forward-tab)
(define-key tau-map (kbd "<header-line> <mouse-5>") 'tabbar-forward-tab)
(define-key tau-map (kbd "<C-S-iso-lefttab>") 'tabbar-backward-tab)
(define-key tau-map (kbd "<header-line> <mouse-4>") 'tabbar-backward-tab)
(define-key tau-map (kbd "C-t") 'tabbar-forward-tab)
(define-key tau-map (kbd "C-d") 'mc/mark-next-like-this)
(define-key tau-map (kbd "C-r") 'tabbar-backward-tab)
(define-key tau-map (kbd "C-o") 'menu-find-file-existing)
(define-key tau-map (kbd "C-w") 'tabbar-close-tab)
(define-key tau-map (kbd "C-a") 'mark-whole-buffer)
(define-key tau-map (kbd "C-c") 'tau-copy-mark)
(define-key tau-map (kbd "C-x") 'kill-region)
(define-key tau-map (kbd "C-v") 'yank)
(define-key tau-map (kbd "C-q") 'save-buffers-kill-terminal)
(define-key tau-map (kbd "C-s") 'save-buffer)
(define-key tau-map (kbd "C-S-s") 'write-file)
(define-key tau-map (kbd "C-z") 'undo-tree-undo)
(define-key tau-map (kbd "C-S-z") 'undo-tree-redo)
(define-key tau-map (kbd "C-y") 'undo-tree-recdo)
(define-key tau-map (kbd "C-f") 'counsel-grep-or-swiper)
(define-key tau-map (kbd "C-p") 'counsel-projectile-find-file)
(define-key tau-map (kbd "C-S-f") 'counsel-git-grep)

(define-key mc/keymap (kbd "<return>") nil)
(define-key swiper-map (kbd "C-k e <escape>") 'tau/esc-minibuffer-quit)
(define-key ivy-minibuffer-map (kbd "C-k e <escape>") 'tau/esc-minibuffer-quit)
(define-key minibuffer-local-completion-map (kbd "C-k e <escape>") 'tau/esc-minibuffer-quit)
(define-key minibuffer-local-map (kbd "C-k e <escape>") 'tau/esc-minibuffer-quit)

(define-key swiper-map (kbd "<escape>") 'tau/esc-minibuffer-quit)
(define-key ivy-minibuffer-map (kbd "<escape>") 'tau/esc-minibuffer-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'tau/esc-minibuffer-quit)
(define-key minibuffer-local-map (kbd "<escape>") 'tau/esc-minibuffer-quit)

(define-key ivy-minibuffer-map [remap tau/tab] 'ivy-partial-or-done)
(define-key counsel-find-file-map [remap tau/tab] 'ivy-partial-or-done)
(define-key counsel-projectile-switch-to-buffer-map [remap tau/tab] 'ivy-partial-or-done)

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map [remap tau/tab] nil)))

(provide 'tau-keys)
