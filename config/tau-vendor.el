

 


(use-package async :ensure t)
(use-package diminish :ensure t
  :config (progn
    (diminish 'undo-tree-mode)))

(use-package counsel :ensure t
       :delight)

(use-package smooth-scrolling :ensure t
       :delight)

(use-package expand-region :ensure t
	     :delight)

(use-package neotree :ensure t
       :delight)

(use-package multiple-cursors :ensure t
  :config (progn


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



    ))

(use-package php-mode :ensure t)
(use-package json-mode :ensure t)
(use-package web-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package js2-mode :ensure t)
(use-package gitignore-mode :ensure t)
(use-package git-commit :ensure t)
(use-package gitattributes-mode :ensure t)
(use-package git-messenger :ensure t)
(use-package smartparens :ensure t)
(use-package pip-requirements :ensure t)
(use-package markdown-mode :ensure t)
(use-package go-mode :ensure t)
(use-package sass-mode :ensure t)
(use-package auto-complete :ensure t)

(use-package counsel-projectile :ensure t
  :delight)

(use-package swiper :ensure t
  :delight)

(use-package ivy  :ensure t
  :delight
  :config (progn
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
    (ivy-mode 1)))

(use-package spaceline :ensure t
  :delight
  :config (progn
    (require 'spaceline-config)

    (require 'powerline)
    (setq powerline-height 20)
    (setq powerline-default-separator 'bar)
    (setq spaceline-separator-dir-left '(right . left))
    (setq spaceline-separator-dir-right '(left . right))
    (setq powerline-utf-8-separator-left #x2502)
    (setq powerline-utf-8-separator-right #x2502)
    (setq echo-keystrokes nil)
    (defadvice previous-line (around silencer activate)
      (condition-case nil
        ad-do-it
        ((beginning-of-buffer))))

    (defadvice next-line (around silencer activate)
      (condition-case nil
        ad-do-it
        ((end-of-buffer))))
    (spaceline-toggle-hud-on)

    (spaceline-define-segment hud
      "A HUD that shows which part of the buffer is currently visible."
      (powerline-hud default-face highlight-face)
      :tight t)

    (spaceline-define-segment line-column
      "The current line and column numbers, or `(current page/number of pages)`
    in pdf-view mode (enabled by the `pdf-tools' package)."
      (if (eq major-mode 'pdf-view-mode)
          (spaceline--pdfview-page-number)
        "%l:%c"))

    (spaceline-define-segment buffer-line-abbrev
      "The line ending convention used in the buffer."
      (let ((buf-coding (format "%s" buffer-file-coding-system)))
        (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
            (pcase (match-string 1 buf-coding)
                         ("unix" "LF")
                         ("dos" "CR LF")
                         ("mac" "CR")
                         (_ ""))
          "")))

    (spaceline-define-segment buffer-encoding-abbrev
      "The line ending convention used in the buffer."
      (let ((buf-coding (format "%s" buffer-file-coding-system)))
        (if (string-match "\\(utf-[0-9]+\\|utf-[0-9]+-hfs\\|iso-latin-[0-9]+\\|iso-8859-[0-9]+\\|windows-[0-9]+\\)" buf-coding)
            (match-string 1 buf-coding)
          buf-coding)))

  (spaceline-compile
    `(
      (line-column :skip-alternate t :face default-face)
      (selection-info :when active :skip-alternate t :face highlight-face)
     )
    `(
      (major-mode :skip-alternate t :face highlight-face)
      (buffer-size :skip-alternate t :face default-face)
      (buffer-encoding-abbrev :skip-alternate t :face default-face)
      (buffer-line-abbrev :skip-alternate t :face default-face)
      ;(hud :skip-alternate t :tight t :face default-face :skip-alternate t)
     )
    )
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))))


(require 'tabbar)
(require 'aquamacs-compat)
(require 'aquamacs-tools)
(require 'one-buffer-one-frame)
(require 'aquamacs-tabbar)


 (when (require 'tabbar nil t)
   ;; Enable tabbars globally:
   (tabbar-mode 1)

   ;; I use this minor-mode mainly as a global mode (see below):
   (define-minor-mode tabbar-on-term-only-mode
     "Display tabbar on terminals and buffers in fundamental mode only."
     :init-value t
     :lighter nil
     :keymap nil
     (if tabbar-on-term-only-mode
         ;; filter is enabled
         (if (or (eq major-mode 'neotree-mode)); <- this can be easily customizable...
             (tabbar-local-mode 1)
             (tabbar-local-mode -1)
           )
       ;; always activate tabbar locally when we disable the minor mode:
       (tabbar-local-mode 1)))

   (defun tabbar-on-term-only-mode-on ()
     "Turn on tabbar if current buffer is a terminal."
     (unless (minibufferp) (tabbar-on-term-only-mode 1)))

   ;; Define a global switch for the mode. Note that this is not set for buffers
   ;; in fundamental mode.
   ;;
   ;; I use it 'cause some major modes do not run the
   ;; `after-change-major-mode-hook'...
   (define-globalized-minor-mode global-tabbar-on-term-only-mode
     tabbar-on-term-only-mode tabbar-on-term-only-mode-on)

   ;; Eventually, switch on this global filter for tabbars:
(global-tabbar-on-term-only-mode 1))

(setq tabbar-auto-scroll-flag t)

    (setq tabbar-buffer-groups-function
          (lambda () (list "All Buffers")))
    (setq tabbar-buffer-list-function
          (lambda ()
            (remove-if
             (lambda(buffer)
               (find (aref (buffer-name buffer) 0) " *"))
             (buffer-list))))


  (defcustom tabbar-hide-header-button t
    "Hide header button at left-up corner.
Default is t."
    :type 'boolean
    :set (lambda (symbol value)
           (set symbol value)
           (if value
               (setq
                tabbar-scroll-left-help-function nil ;don't show help information
                tabbar-scroll-right-help-function nil
                tabbar-help-on-tab-function nil
                tabbar-home-help-function nil
                tabbar-buffer-home-button (quote (("") "")) ;don't show tabbar button
                tabbar-scroll-left-button (quote ((" « ") " « "))
                tabbar-scroll-right-button (quote ((" » ") " » ")))))
    :group 'tabbar)

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))

(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

(add-hook 'emacs-startup-hook
          (lambda () (delete-other-windows)) t)

(provide 'tau-vendor)
