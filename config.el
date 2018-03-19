(setq auto-save-default nil)
(use-package spaceline :ensure t)
(require 'tau-fonts)
(require 'tau-interface)
(require 'tau-editor)
(require 'tau-mouse)

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
(require 'mouse)

(setq use-dialog-box t)

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

(advice-add 'mc/skip-to-next-like-this
          :before
          '(lambda()(interactive)
             (when (> (mc/num-cursors) 1)
               (mc/cycle-backward))))

(defun mc/keyboard-quit ()
  "Deactivate mark if there are any active, otherwise exit multiple-cursors-mode."
  (interactive)
  (if (not (use-region-p))
      (multiple-cursors-mode 0)
    (deactivate-mark))
    (keyboard-escape-quit)
  )
(defun mc/cursor-is-bar ()
  "Return non-nil if the cursor is a bar."
  nil)


(setq ido-mode nil)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

(projectile-mode 1)
(ivy-mode 1)
(delete-selection-mode 1)
(popwin-mode 1)
(undo-tree-mode 1)

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


(defun counsel-projectile-ag-function (string extra-ag-args)
  "Grep in the current directory for STRING.
If non-nil, EXTRA-AG-ARGS string is appended to `counsel-ag-base-command'."
  (when (null extra-ag-args)
    (setq extra-ag-args ""))
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((default-directory (projectile-project-root))
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (let ((ag-cmd (format counsel-ag-base-command
                            (concat extra-ag-args
                                    " -- "
                                    (shell-quote-argument regex)))))
        (counsel--async-command ag-cmd))
      nil)))

(defun counsel-projectile-ag (&optional initial-input initial-directory extra-ag-args ag-prompt)
  "Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS string, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument. "
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name (concat
                                 (car (split-string counsel-ag-base-command))
                                 " in directory: ")))))
  (ivy-set-prompt 'counsel-ag counsel-prompt-function)
  (setq counsel--git-grep-dir (or initial-directory default-directory))
  (ivy-read (or ag-prompt (car (split-string counsel-ag-base-command)))
            (lambda (string)
              (counsel-projectile-ag-function string extra-ag-args))
            :initial-input initial-input
            :dynamic-collection t
            :keymap counsel-ag-map
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'counsel-ag))



(ivy-set-actions
 'counsel-projectile-find-file
 '(("j" (lambda (x)
          (with-ivy-window
            (find-file-other-window
             (projectile-expand-root x))))
    "other window")))


(defvar tau-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq mac-control-modifier 'control)
    (setq mac-shift-modifier 'shift)
    (define-key map (kbd "C-S-<up>")   'drag-stuff-up)
    (define-key map (kbd "C-S-<down>")   'drag-stuff-down)
    (define-key map (kbd "<escape>") 'mc/keyboard-quit)
    (define-key map (kbd "ESC ESC") 'mc/keyboard-quit)
    (define-key map (kbd "C-c") 'kill-ring-save-keep-highlight)
    (define-key map (kbd "s-c") 'kill-ring-save-keep-highlight)
    (define-key map (kbd "C-v") 'yank)
    (define-key map (kbd "s-v") 'yank)
    (define-key map (kbd "C-d") 'mc/mark-next-like-this)
    (define-key map (kbd "s-d") 'mc/mark-next-like-this)
    (define-key map (kbd "C-x") 'kill-region)
    (define-key map (kbd "s-x") 'kill-region)
    (define-key map (kbd "C-k C-b") 'neotree-toggle)
    (define-key map (kbd "s-k s-b") 'neotree-toggle)
    (define-key map (kbd "C-S-p") 'counsel-M-x)
    (define-key map (kbd "s-P") 'counsel-M-x)
    (define-key map (kbd "C-p") 'counsel-projectile-find-file)
    (define-key map (kbd "s-p") 'counsel-projectile-find-file)
    (define-key map (kbd "C-z") 'undo-tree-undo)
    (define-key map (kbd "C-S-z") 'undo-tree-redo)
    (define-key map (kbd "s-z") 'undo-tree-undo)
    (define-key map (kbd "s-Z") 'undo-tree-redo)
    (define-key map (kbd "M-x") 'counsel-M-x)
    (define-key map (kbd "C-a") 'mark-whole-buffer)
    (define-key map (kbd "C-s") 'save-buffer)
    (define-key map (kbd "C-f") 'swiper)
    (define-key map (kbd "s-a") 'mark-whole-buffer)
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
    (define-key map (kbd "C-S-s") 'write-file)
    (define-key map (kbd "s-S") 'write-file)
    (define-key map (kbd "C-S-n") 'new-frame)
    (define-key map (kbd "s-N") 'new-frame)
    (define-key map (kbd "C-S-w") 'delete-frame)
    (define-key map (kbd "s-W") 'delete-frame)
    (define-key map (kbd "C-S-t") 'reopen-killed-file)
    (define-key map (kbd "s-T") 'reopen-killed-file)
    (define-key map (kbd "C-w") 'tabbar-close-tab)
    (define-key map (kbd "s-w") 'tabbar-close-tab)
    (define-key map (kbd "C-<f4>") 'tabbar-close-tab)
    (define-key map (kbd "s-<f4>") 'tabbar-close-tab)
    (define-key map (kbd "C--")   'text-scale-decrease)
    (define-key map (kbd "s--")   'text-scale-decrease)
    (define-key map (kbd "C-+")   'text-scale-increase)
    (define-key map (kbd "s-+")   'text-scale-increase)
    (define-key map (kbd "C-=")   'text-scale-increase)
    (define-key map (kbd "s-=")   'text-scale-increase)
    (define-key map (kbd "C-0")   'friendly-text-scale-reset)
    (define-key map (kbd "s-0")   'friendly-text-scale-reset)
    (define-key map (kbd "C-<tab>") 'tabbar-forward-tab)
    (define-key map (kbd "C-S-<tab>")'tabbar-backward-tab)
    (define-key map (kbd "C-<iso-lefttab>")'tabbar-backward-tab)
    (define-key map (kbd "C-<next>") 'tabbar-forward)
    (define-key map (kbd "C-<prior>")'tabbar-backward)
    (define-key map (kbd "\220") 'counsel-M-x)
    (define-key map (kbd "\232") 'undo-tree-redo)
    (define-key map (kbd "\206") 'counsel-projectile-ag)
    (define-key map [remap write-file] nil)
    (define-key mc/keymap (kbd "<return>") 'newline-and-indent)
    (define-key mc/keymap (kbd "<escape>") 'mc/keyboard-quit)
    map)
  "tau-minor-mode keymap.")

(define-minor-mode tau-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " tau")

(tau-minor-mode 1)
(define-key global-map [menu-bar file] nil)
(define-key global-map [menu-bar edit] nil)
(define-key global-map [menu-bar options] nil)
(define-key global-map [menu-bar help-menu] nil)
(define-key global-map [menu-bar buffer] nil)
(define-key global-map [menu-bar tools] nil)
(define-key text-mode-map [menu-bar text] nil)
(define-key minibuffer-local-completion-map [menu-bar minibuf] nil)
(define-key minibuffer-local-map [menu-bar minibuf] nil)
