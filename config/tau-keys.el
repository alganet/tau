; Command
(setq mac-command-modifier 'super)

;; C-k
(define-key tau-map "\C-k" nil)
(define-key tau-map "\C-k\ e" nil)
(define-key tau-map (kbd "C-k e <escape>") 'keyboard-quit)

; TAB
(define-key tau-map [tab] 'tau/tab)
(define-key tau-map "\t" 'tau/tab)
(define-key tau-map (kbd "<backtab>") 'tau/backtab)
(define-key tau-map (kbd "<S-iso-lefftab>") 'tau/backtab)

; tau-P: Command List
(define-key tau-map (kbd "C-k C-l") 'counsel-M-x)
(define-key tau-map "\C-k\ el" 'counsel-M-x)

; tau-B: Project Browser
(define-key tau-map (kbd "C-k C-b") 'neotree-toggle)
(define-key tau-map "\C-k\ eb" 'neotree-toggle)

; Windows and Tabs

(define-key tau-map (kbd "<C-tab>") 'tabbar-forward-tab)
(define-key tau-map (kbd "<header-line> <mouse-5>") 'tabbar-forward-tab)
(define-key tau-map (kbd "<C-S-iso-lefttab>") 'tabbar-backward-tab)
(define-key tau-map (kbd "<header-line> <mouse-4>") 'tabbar-backward-tab)
(define-key tau-map (kbd "C-k C-n") 'tabbar-forward-tab)
(define-key tau-map (kbd "C-k C-p") 'tabbar-backward-tab)
(define-key tau-map (kbd "C-w") 'tabbar-close-tab)
(define-key tau-map (kbd "s-w") 'tabbar-close-tab)
(define-key tau-map (kbd "C-n") 'new-tab)
(define-key tau-map (kbd "s-n") 'new-tab)
(define-key tau-map (kbd "C-S-s") 'write-file)
(define-key tau-map (kbd "C-q") 'save-buffers-kill-terminal)
(define-key tau-map (kbd "s-q") 'save-buffers-kill-terminal)
(define-key tau-map (kbd "C-o") 'menu-find-file-existing)
(define-key tau-map (kbd "s-o") 'menu-find-file-existing)

(define-key tau-map (kbd "C-k C-d") 'delete-window)
(define-key tau-map (kbd "C-k C-o") 'other-window)
(define-key tau-map (kbd "C-k C-h") 'split-window-horizontally)
(define-key tau-map (kbd "C-k C-v") 'split-window-vertically)
(define-key tau-map (kbd "C-k C-<up>") 'windmove-up)
(define-key tau-map (kbd "C-k C-<down>") 'windmove-down)
(define-key tau-map (kbd "C-k C-<left>") 'windmove-left)
(define-key tau-map (kbd "C-k C-<right>") 'windmove-right)
(define-key tau-map (kbd "C-k <up>") 'windmove-up)
(define-key tau-map (kbd "C-k <down>") 'windmove-down)
(define-key tau-map (kbd "C-k <left>") 'windmove-left)
(define-key tau-map (kbd "C-k <right>") 'windmove-right)

; Selection and cursor movement
(define-key tau-map (kbd "C-S-<up>") 'move-region-up)
(define-key tau-map (kbd "C-S-<down>") 'move-region-down)
(define-key tau-map (kbd "C-S-d") 'duplicate-current-line-or-region)
(define-key tau-map (kbd "C-S-k") 'remove-current-line-or-region)
(define-key tau-map (kbd "C-d") 'mc/mark-next-like-this)
(define-key tau-map (kbd "s-C-<up>") 'move-region-up)
(define-key tau-map (kbd "s-C-<down>") 'move-region-down)
(define-key tau-map (kbd "s-D") 'duplicate-current-line-or-region)
(define-key tau-map (kbd "s-d") 'mc/mark-next-like-this)
(define-key tau-map (kbd "s-<left>") 'smart-beginning-of-line)
(define-key tau-map (kbd "s-<right>") 'smart-end-of-line)

; Universal keys
(define-key tau-map (kbd "C-a") 'mark-whole-buffer)
(define-key tau-map (kbd "C-c") 'tau-copy-mark)
(define-key tau-map (kbd "C-x") 'kill-region)
(define-key tau-map (kbd "C-v") 'yank)
(define-key tau-map (kbd "C-z") 'undo-tree-undo)
(define-key tau-map (kbd "C-S-z") 'undo-tree-redo)
(define-key tau-map (kbd "C-y") 'undo-tree-redo)
(define-key tau-map (kbd "C-f") 'counsel-grep-or-swiper)
(define-key tau-map (kbd "C-p") 'counsel-projectile-find-file)
(define-key tau-map (kbd "C-S-f") 'counsel-git-grep)
(define-key tau-map (kbd "C-g") 'goto-line)
(define-key tau-map (kbd "s-a") 'mark-whole-buffer)
(define-key tau-map (kbd "s-c") 'tau-copy-mark)
(define-key tau-map (kbd "s-x") 'kill-region)
(define-key tau-map (kbd "s-v") 'yank)
(define-key tau-map (kbd "s-s") 'save-buffer)
(define-key tau-map (kbd "s-z") 'undo-tree-undo)
(define-key tau-map (kbd "s-Z") 'undo-tree-redo)
(define-key tau-map (kbd "s-y") 'undo-tree-redo)
(define-key tau-map (kbd "s-f") 'counsel-grep-or-swiper)
(define-key tau-map (kbd "s-p") 'counsel-projectile-find-file)
(define-key tau-map (kbd "s-F") 'counsel-git-grep)
(define-key tau-map (kbd "s-g") 'goto-line)

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
