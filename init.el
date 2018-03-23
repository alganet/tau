;; tau init.el
;;
;; Configures the tau editor on top of emacs
;;

;; Use mode line to display startup messages
(setq mode-line-format "Initializing...")

;; Path fix for OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Window Title during loading
(setq frame-title-format (format "tau"))

;; No dialog boxes
(setq use-dialog-box nil)

;; Setup desired modes on GUI mode
(if window-system
  (scroll-bar-mode -1))

;; Use mouse
(mouse-wheel-mode t)
(xterm-mouse-mode t)

;; Turn off unwanted modes when they're available
(if (functionp 'menu-bar-mode)
  (menu-bar-mode -1))
(if (functionp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Be quiet
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-buffer-choice "untitled")
(setq initial-major-mode 'text-mode)
(setq inhibit-startup-echo-area-message t)
(setq-default message-log-max nil)
(setq tabbar-auto-scroll-flag t)

;; Use nice colors
(setq ns-use-srgb-colorspace nil)

;; Tweak garbage collector
(setq gc-cons-threshold 20000000)

;; Define user configuration directory
(setq user-config-directory
  (expand-file-name "config/" user-emacs-directory))

;; Define themes directory
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

;; Load themes from config directory
(add-to-list 'custom-theme-load-path user-theme-directory)

;; Load elpa packages from vendor dir
(setq package-user-dir
  (expand-file-name "vendor/elpa" user-emacs-directory))

;; Load quelpa packages from vendor dir
(setq quelpa-dir
  (expand-file-name "vendor/quelpa" user-emacs-directory))


(setq mode-line-format "Initializing packages...")

;; Initialize packaging system
(package-initialize)

;; Setup melpa
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

;;; Bind quelpa to use-package
(require 'quelpa)
(require 'quelpa-use-package)
(quelpa-use-package-activate-advice)

;; Set Window title after loading
(setq frame-title-format (format "%%f - Tau" (system-name)))

(setq mode-line-format "Initializing configuration...")

;; Loads User Configuration
(eval-when-compile (load-file user-config-file))

;; Setups custom file
(setq custom-file "~/.emacs.d/custom.el")

(setq mode-line-format "Initializing tau...")

;; tau-Fu Time!!!

(provide 'init)
