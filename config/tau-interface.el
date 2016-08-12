(setq frame-title-format (format "%%f - Tau" (system-name)))
(setq use-dialog-box t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(setq inhibit-startup-echo-area-message t)
(setq-default message-log-max nil)
(setq tabbar-auto-scroll-flag t)
(setq ns-use-srgb-colorspace nil)

(defun display-startup-echo-area-message ()
  (message ""))

(defadvice read-file-name (around read-file-name act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
    ad-do-it))

(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
    ad-do-it))

(if window-system
    (tool-bar-mode -1))

(eval-after-load 'help-mode
  (lambda ()
    (define-key help-mode-map [menu-bar help-mode] nil)
    (define-key help-mode-map [menu-bar Help-Mode] nil)
    ))


(unless (require 'aquamacs-tabbar nil t)
  (use-package aquamacs-tabbar
    :ensure t
    :load-path "quelpa/build/aquamacs-tabbar/"
    :quelpa (aquamacs-tabbar :fetcher github :repo "alganet/tabbar")))
(use-package monokai-theme :ensure t)

(require 'aquamacs-tabbar)


(defadvice kill-buffer (before kill-buffer-before-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      )))

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

(neotree-toggle)
(new-empty-buffer)

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

(add-hook 'emacs-startup-hook
          (lambda () (delete-other-windows)) t)

(defun spaceline-tau-theme (&rest additional-segments)
  "Install the modeline used by Spacemacs.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (apply 'spaceline--theme
         '((persp-name
            workspace-number
            window-number)
           :fallback evil-state
           :separator "|"
           :face highlight-face)
         '(buffer-modified buffer-size buffer-id remote-host)
         additional-segments))


(require 'spaceline-config)
(setq powerline-default-separator 'zigzag)
(setq powerline-height 26)
(spaceline-toggle-buffer-id-off)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-buffer-modified-off)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-buffer-encoding-on)
(spaceline-toggle-buffer-position-off)
(spaceline-toggle-hud-on)
(load-theme 'monokai t)

(spaceline-define-segment hud
  "A HUD that shows which part of the buffer is currently visible."
  (powerline-hud highlight-face default-face)
  :tight t)

(spaceline-tau-theme)

(spaceline-compile)


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
         (if (eq major-mode 'neotree-mode); <- this can be easily customizable...
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


(menu-bar-mode -1)

(provide 'tau-interface)
