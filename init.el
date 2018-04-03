(setq package-enable-at-startup nil)
(setq gc-cons-threshold 10000000)
(setq warning-minimum-level :warning)
(setq ns-use-srgb-colorspace nil)
(setq-default message-log-max nil)
(setq initial-major-mode 'markdown-mode)
(setq initial-scratch-message nil)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq frame-title-format (format "ταυ"))
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq use-dialog-box nil)
(setq custom-safe-themes t)

(setq-default header-line-format '("%e ταυ "))
(setq-default mode-line-format '("%e ταυ is starting"))
(redisplay t)

(if window-system
  (scroll-bar-mode -1)
)

(if window-system
  (mouse-wheel-mode t)
  (xterm-mouse-mode t)
)

(if (functionp 'menu-bar-mode)
  (menu-bar-mode -1)
)

(if (functionp 'tool-bar-mode)
  (tool-bar-mode -1)
)

(require 'package)

;; Setups custom file
(setq custom-file
  (expand-file-name "custom.el" user-emacs-directory))

;; Define user configuration directory
(setq user-config-directory
  (expand-file-name "config/" user-emacs-directory))
;; Define user configuration directory
(setq tabbar-directory
  (expand-file-name "config/tabbar" user-emacs-directory))

;; Define themes directory
(setq user-theme-directory
  (expand-file-name "config/themes" user-emacs-directory))

;; Create user configuration directory if not exists
(unless (file-exists-p user-config-directory)
  (make-directory user-config-directory))

;; Load packages from user configuration directory
(add-to-list 'load-path user-config-directory)
(add-to-list 'load-path tabbar-directory)

;; Load themes from config directory
(add-to-list 'custom-theme-load-path user-theme-directory)

;; Maximize on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Load elpa packages from vendor dir
(setq package-user-dir
  (expand-file-name "vendor/elpa" user-emacs-directory))

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))


(setq-default mode-line-format '("%e τau is setting up"))
(redisplay t)

(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(require 'tau-core)
