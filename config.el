(require 'tau-theme)
(load-theme 'tau t)

;; ---------------

(setq auto-save-default nil)

(setq mode-line-format "Initializing packages...")
(use-package swiper :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package counsel :ensure t)
(use-package ivy :ensure t)
(use-package avoid :ensure t)
(use-package multiple-cursors :ensure t)
(use-package drag-stuff :ensure t)
(use-package popwin :ensure t)
(use-package undo-tree :ensure t)
(use-package projectile :ensure t)
(use-package counsel-projectile :ensure t)
(use-package php-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package neotree :ensure t)
(use-package spaceline :ensure t)
(unless (require 'aquamacs-tabbar nil t)
  (use-package aquamacs-tabbar
    :ensure t
    :load-path "quelpa/build/aquamacs-tabbar/"
    :quelpa (aquamacs-tabbar :fetcher github :repo "alganet/tabbar")))


(require 'undo-tree)
(require 'vc)
(require 'projectile)
(require 'counsel-projectile)
(require 'swiper)
(require 'counsel)
(require 'ivy)
(require 'avoid)
(require 'multiple-cursors)
(require 'drag-stuff)
(require 'popwin)
(require 'php-mode)
(require 'neotree)
(require 'aquamacs-tabbar)
(require 'term)

;; disable cua and transient mark modes in term-char-mode
(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil))
(ad-activate 'term-char-mode)

(setq mode-line-format "")

(defun kill-ring-save-keep-highlight (beg end)
  "Keep the region active after the kill"
  (interactive "r")
  (prog1 (kill-ring-save beg end)
    (setq deactivate-mark nil)))

(advice-add 'mc/mark-next-like-this
          :before
          '(lambda(arg)(interactive "p")
             (when (< arg 0)
               (mc/cycle-backward))))

(advice-add 'mc/mark-next-like-this
          :after
          '(lambda(arg)(interactive "p")
             (unless (< arg 0)
               (mc/cycle-forward))))

(defun mc/keyboard-quit ()
  "Deactivate mark if there are any active, otherwise exit multiple-cursors-mode."
  (interactive)
    (if (not (use-region-p))
      (multiple-cursors-mode 0)
      (deactivate-mark))
    (keyboard-escape-quit))

(defun mc/cursor-is-bar ()
  "Return non-nil if the cursor is a bar."
  nil)

(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when killed-file-list
    (find-file (pop killed-file-list))))
(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun friendly-text-scale-reset ()
  "Set the height of the default face in the current buffer to its default value."
  (interactive)
  (text-scale-increase 0))

(ivy-set-actions
 'counsel-projectile-find-file
 '(("j" (lambda (x)
          (with-ivy-window
            (find-file-other-window
             (projectile-expand-root x))))
    "other window")))


(setq ido-mode nil)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

(projectile-mode 1)
(ivy-mode 1)
(delete-selection-mode 1)
(popwin-mode 1)
(global-undo-tree-mode 1)

(defvar tau-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<escape>") 'mc/keyboard-quit)
    (define-key map (kbd "ESC ESC") 'mc/keyboard-quit)
    (define-key map (kbd "C-c") 'kill-ring-save-keep-highlight)
    (define-key map (kbd "C-d") 'mc/mark-next-like-this)
    (define-key map (kbd "C-S-<down>")   'drag-stuff-down)
    (define-key map (kbd "C-s") 'save-buffer)
    (define-key map (kbd "C-S-s") 'write-file)
    (define-key map (kbd "C-S-<up>")   'drag-stuff-up)
    (define-key map (kbd "C-S-z") 'undo-tree-redo)
    (define-key map (kbd "C-v") 'yank)
    (define-key map (kbd "C-x") 'kill-region)
    (define-key map (kbd "C-z") 'undo-tree-undo)
    (define-key map (kbd "s-c") 'kill-ring-save-keep-highlight)
    (define-key map (kbd "s-d") 'mc/mark-next-like-this)
    (define-key map (kbd "s-S") 'write-file)
    (define-key map (kbd "s-v") 'yank)
    (define-key map (kbd "s-x") 'kill-region)
    (define-key map (kbd "s-Z") 'undo-tree-redo)
    (define-key map (kbd "s-z") 'undo-tree-undo)
    (define-key map [remap write-file] nil)
    (define-key mc/keymap (kbd "<return>") 'newline-and-indent)
    map)
  "tau-minor-mode keymap.")

(defvar tau-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq mac-control-modifier 'control)
    (setq mac-shift-modifier 'shift)
    (define-key map (kbd "<f12>") 'tau-term-open)
    (define-key map (kbd "<escape>") 'mc/keyboard-quit)
    (define-key map (kbd "ESC ESC") 'mc/keyboard-quit)
    (define-key map (kbd "C-S-p") 'counsel-M-x)
    (define-key map (kbd "s-P") 'counsel-M-x)
    (define-key map (kbd "C-p") 'counsel-projectile-find-file)
    (define-key map (kbd "s-p") 'counsel-projectile-find-file)
    (define-key map (kbd "M-x") 'counsel-M-x)
    (define-key map (kbd "C-f") 'swiper)
    (define-key map (kbd "s-a") 'mark-whole-buffer)
    (define-key map (kbd "C-a") 'mark-whole-buffer)
    (define-key map (kbd "s-s") 'save-buffer)
    (define-key map (kbd "s-f") 'swiper)
    (define-key map (kbd "C-S-f") 'counsel-projectile-ag)
    (define-key map (kbd "s-F") 'counsel-projectile-ag)
    (define-key map (kbd "C-o") 'menu-find-file-existing)
    (define-key map (kbd "s-o") 'menu-find-file-existing)
    (define-key map (kbd "C-q") 'kill-emacs)
    (define-key map (kbd "s-q") 'kill-emacs)
    (define-key map (kbd "C-n") 'new-empty-buffer)
    (define-key map (kbd "s-n") 'new-empty-buffer)
    (define-key map (kbd "C-S-n") 'new-frame)
    (define-key map (kbd "s-N") 'new-frame)
    (define-key map (kbd "C-S-w") 'delete-frame)
    (define-key map (kbd "s-W") 'delete-frame)
    (define-key map (kbd "C-S-t") 'reopen-killed-file)
    (define-key map (kbd "s-T") 'reopen-killed-file)
    (define-key map (kbd "C-<f4>") 'tabbar-close-tab)
    (define-key map (kbd "s-<f4>") 'tabbar-close-tab)
    (define-key map (kbd "C-w") 'tabbar-close-tab)
    (define-key map (kbd "s-w") 'tabbar-close-tab)
    (define-key map (kbd "C--")   'text-scale-decrease)
    (define-key map (kbd "s--")   'text-scale-decrease)
    (define-key map (kbd "C-+")   'text-scale-increase)
    (define-key map (kbd "s-+")   'text-scale-increase)
    (define-key map (kbd "C-=")   'text-scale-increase)
    (define-key map (kbd "s-=")   'text-scale-increase)
    (define-key map (kbd "C-0")   'friendly-text-scale-reset)
    (define-key map (kbd "s-0")   'friendly-text-scale-reset)
    (define-key map (kbd "C-k C-b") 'neotree-toggle)
    (define-key map (kbd "s-k s-b") 'neotree-toggle)
    (define-key map (kbd "C-k C-n") 'tabbar-forward-tab)
    (define-key map (kbd "s-k s-n") 'tabbar-forward-tab)
    (define-key map (kbd "C-k C-p") 'tabbar-backward-tab)
    (define-key map (kbd "s-k s-p") 'tabbar-backward-tab)
    (define-key map (kbd "C-k C-f") 'counsel-projectile-ag)
    (define-key map (kbd "s-k s-f") 'counsel-projectile-ag)
    (define-key map (kbd "C-<tab>") 'tabbar-forward-tab)
    (define-key map (kbd "C-S-<tab>")'tabbar-backward-tab)
    (define-key map (kbd "C-<iso-lefttab>")'tabbar-backward-tab)
    (define-key map (kbd "C-<next>") 'tabbar-forward)
    (define-key map (kbd "C-<prior>")'tabbar-backward)
    (define-key map (kbd "\220") 'counsel-M-x)
    (define-key map (kbd "\232") 'undo-tree-redo)
    (define-key map (kbd "\206") 'counsel-projectile-ag)
    (define-key map [remap write-file] nil)
    map)
  "tau-ui-mode keymap.")

(define-minor-mode tau-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " tau")

(define-minor-mode tau-ui-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " tau")

(define-minor-mode tau-shell-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value -1
  :lighter " tau")

(define-key global-map (kbd "C-k") nil)
(define-key global-map [menu-bar file] nil)
(define-key global-map [menu-bar edit] nil)
(define-key global-map [menu-bar options] nil)
(define-key global-map [menu-bar help-menu] nil)
(define-key global-map [menu-bar buffer] nil)
(define-key global-map [menu-bar tools] nil)
(define-key text-mode-map [menu-bar text] nil)
(define-key minibuffer-local-completion-map [menu-bar minibuf] nil)
(define-key minibuffer-local-map [menu-bar minibuf] nil)
(define-key projectile-command-map [escape] 'minibuffer-keyboard-quit)
(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
(define-key swiper-map [escape] 'minibuffer-keyboard-quit)
(define-key term-mode-map (kbd "<escape>") 'term-send-ESC)
(define-key term-mode-map (kbd "ESC ESC") 'term-send-ESC)
(define-key term-mode-map (kbd "C-d") 'term-send-C-d)
(define-key term-mode-map (kbd "C-c") 'term-send-C-c)
(define-key term-mode-map (kbd "C-r") 'term-send-C-r)
(define-key term-mode-map (kbd "C-z") 'term-send-C-z)
(define-key term-mode-map (kbd "C-SPC") 'term-send-C-SPC)

(defun term-send-C-SPC ()
  "Send C-SPC in term mode."
  (interactive)
  (term-send-raw-string "\C- "))

(defun term-send-C-d ()
  "Send C-d in term mode."
  (interactive)
  (term-send-raw-string "\C-d"))

(defun term-send-C-c ()
  "Send C-c in term mode."
  (interactive)
  (term-send-raw-string "\C-c"))

(defun term-send-C-r ()
  "Send C-r in term mode."
  (interactive)
  (term-send-raw-string "\C-r"))

(defun term-send-C-z ()
  "Send C-z in term mode."
  (interactive)
  (term-send-raw-string "\C-z"))

(defun term-send-ESC ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun tau-term-open ()
  (interactive)
  (if (get-buffer "*Terminal*")
    (switch-to-buffer "*Terminal*")
    (ansi-term "sh" "Terminal")))

(tau-ui-mode 1)
(tau-minor-mode 1)

(require 'tau-interface)
(require 'tau-fonts)
(require 'tau-editor)
(require 'tau-mouse)