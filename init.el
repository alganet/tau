;; tau init.el
;;
;; Configures the tau editor on top of emacs
;;

;; Turn off unwanted modes
(tool-bar-mode -1)
(menu-bar-mode -1)


(set-face-attribute 'mode-line nil  :height 85)
(defun my-minibuffer-setup ()
       (set (make-local-variable 'face-remapping-alist)
          '((default :height 0.9))))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)

;; Use mode line to display startup messages
 (setq mode-line-format "Initializing...")

;; Path fix for OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Window Title
(setq frame-title-format (format "tau" (system-name)))

;; No dialog boxes
(setq use-dialog-box nil)

(if window-system
  (mouse-wheel-mode t)
  (xterm-mouse-mode t))

(if window-system
    (scroll-bar-mode -1))

;; Be quiet
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(setq inhibit-startup-echo-area-message t)
(setq-default message-log-max nil)
(setq tabbar-auto-scroll-flag t)

;; Use nice colors
(setq ns-use-srgb-colorspace nil)

;; Performance
(setq gc-cons-threshold 20000000)

;; Define user configuration directory
(setq user-config-directory
  (expand-file-name "config/" user-emacs-directory))

(setq user-theme-directory
  (expand-file-name "config/" user-emacs-directory))

;; Define user configuration file
(setq user-config-file
  (expand-file-name "config.el" user-emacs-directory))



;; Create user configuration directory if not exists
(unless (file-exists-p user-config-directory)
  (make-directory user-config-directory))

;; Load packages from user configuration directory
(add-to-list 'load-path user-config-directory)
(add-to-list 'custom-theme-load-path user-theme-directory)

(setq mode-line-format "Initializing packages...")

;; Initialize packaging system
(package-initialize)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))

;; Install use-package if not available
(unless (require 'use-package nil t)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

;; Use packages from alternative sources
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; Manage packages from alternative sources with use-package
(unless (require 'quelpa-use-package nil t)
  (quelpa
   '(quelpa-use-package
     :fetcher github
     :upgrade nil
     :repo "quelpa/quelpa-use-package")))

(require 'quelpa)
(require 'quelpa-use-package)
(quelpa-use-package-activate-advice)

(setq frame-title-format (format "%%f - Tau" (system-name)))

;; Loads User Configuration
(eval-when-compile (load-file user-config-file))

(setq mode-line-format "Initializing configuration...")

(setq custom-file "~/.emacs.d/custom.el")

;; tau-Fu Time!!!

(setq mode-line-format "Initializing tau...")

(provide 'init)
