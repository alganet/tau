(setq frame-title-format (format "%%f - Tau" (system-name)))
(setq use-dialog-box t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(setq inhibit-startup-echo-area-message t)
(setq-default message-log-max nil)
(setq tabbar-auto-scroll-flag t)

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

(define-key global-map [menu-bar file] nil)
(define-key global-map [menu-bar edit] nil)
(define-key global-map [menu-bar options] nil)
(define-key global-map [menu-bar help-menu] nil)
(define-key global-map [menu-bar buffer] nil)
(define-key global-map [menu-bar tools] nil)
(define-key text-mode-map [menu-bar text] nil)
(define-key minibuffer-local-completion-map [menu-bar minibuf] nil)
(define-key minibuffer-local-map [menu-bar minibuf] nil)

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

(new-empty-buffer)

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
(tabbar-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(provide 'tau-interface)
