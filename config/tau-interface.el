(setq frame-title-format (format "%%f - Tau" (system-name)))
(setq use-dialog-box t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(setq inhibit-startup-echo-area-message t)
(setq-default message-log-max nil)
(setq tabbar-auto-scroll-flag nil)

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
    :load-path "quelpa/build/aquamacs-tabbar/"
    :quelpa (aquamacs-tabbar :fetcher github :repo "alganet/tabbar")))

(require 'aquamacs-tabbar)
(tabbar-mode t)
(menu-bar-mode -1)

(load-theme 'monokai t)

(provide 'tau-interface)
