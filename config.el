(use-package spaceline :ensure t)
(require 'tau-fonts)
(require 'tau-interface)
(require 'tau-editor)
(require 'tau-mouse)

(use-package swiper :ensure t)
(use-package counsel :ensure t)
(use-package ivy :ensure t)
(use-package avoid :ensure t)
(use-package multiple-cursors :ensure t)
(use-package drag-stuff :ensure t)
(use-package popwin :ensure t)
(use-package php-mode :ensure t)

(require 'swiper)
(require 'counsel)
(require 'ivy)
(require 'avoid)
(require 'multiple-cursors)
(require 'drag-stuff)
(require 'popwin)
(require 'php-mode)

(setq use-dialog-box t)

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

(ivy-mode 1)
(delete-selection-mode 1)
(popwin-mode 1)


(defun friendly-locate-function (input)
  (if (< (length input) 3)
      (counsel-more-chars 3)
    (counsel--async-command
     (apply 'ag/search string (ag/project-root)))
    '("" "working...")))

;;;###autoload
(defun friendly-locate (&optional initial-input)
  "Call the \"locate\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read "Locate: " #'counsel-locate-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-locate-history
            :action (lambda (file)
                      (with-ivy-window
                        (when file
                          (find-file file))))
            :unwind #'counsel-delete-process
            :caller 'counsel-locate))

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

(defvar tau-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-S-<up>")   'drag-stuff-up)
    (define-key map (kbd "C-S-<down>")   'drag-stuff-down)
    (define-key map (kbd "<escape>") 'mc/keyboard-quit)
    (define-key map (kbd "C-c") 'kill-ring-save-keep-highlight)
    (define-key map (kbd "C-v") 'yank)
    (define-key map (kbd "C-d") 'mc/mark-next-like-this)
    (define-key map (kbd "C-x") 'kill-region)
    (define-key map (kbd "C-S-p") 'counsel-M-x)
    (define-key map (kbd "C-p") 'friendly-locate)
    (define-key map (kbd "C-z") 'undo)
    (define-key map (kbd "C-S-z") 'redo)
    (define-key map (kbd "M-x") 'counsel-M-x)
    (define-key map (kbd "C-a") 'mark-whole-buffer)
    (define-key map (kbd "C-s") 'save-buffer)
    (define-key map (kbd "C-f") 'swiper)
    (define-key map (kbd "C-S-f") 'counsel-ag)
    (define-key map (kbd "C-o") 'menu-find-file-existing)
    (define-key map (kbd "C-q") 'kill-emacs)
    (define-key map (kbd "C-n") 'new-empty-buffer)
    (define-key map (kbd "C-S-s") 'write-file)
    (define-key map (kbd "C-S-n") 'new-frame)
    (define-key map (kbd "C-S-w") 'delete-frame)
    (define-key map (kbd "C-S-t") 'reopen-killed-file)
    (define-key map (kbd "C-<tab>") 'tabbar-forward-tab)
    (define-key map (kbd "C-S-<tab>")'tabbar-backward-tab)
    (define-key map (kbd "C-<iso-lefttab>")'tabbar-backward-tab)
    (define-key map (kbd "C-<next>") 'tabbar-forward)
    (define-key map (kbd "C-<prior>")'tabbar-backward)
    (define-key map (kbd "C-w") 'tabbar-close-tab)
    (define-key map (kbd "C-<f4>") 'tabbar-close-tab)
    (define-key map (kbd "C--")   'text-scale-decrease)
    (define-key map (kbd "C-+")   'text-scale-increase)
    (define-key map (kbd "C-=")   'text-scale-increase)
    (define-key map (kbd "C-0")   'friendly-text-scale-reset)
    (define-key map [remap write-file] nil)
    (define-key mc/keymap (kbd "<return>") 'newline-and-indent)
    map)
  "tau-minor-mode keymap.")

(define-minor-mode tau-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " tau")

(tau-minor-mode 1)
