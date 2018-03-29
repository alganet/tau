(setq gc-cons-threshold 10000000)
(setq warning-minimum-level :emergency)
(setq-default message-log-max nil)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)
(setq initial-buffer-choice "untitled.txt")
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq frame-title-format (format "tau"))
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq use-dialog-box nil)
(setq custom-safe-themes t)

(setq mode-line-format '(" τau is starting :) "))

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
  (expand-file-name "config/" user-emacs-directory))

;; Create user configuration directory if not exists
(unless (file-exists-p user-config-directory)
  (make-directory user-config-directory))

;; Load packages from user configuration directory
(add-to-list 'load-path user-config-directory)
(add-to-list 'load-path tabbar-directory)

;; Load themes from config directory
(add-to-list 'custom-theme-load-path user-theme-directory)

;; Load elpa packages from vendor dir
(setq package-user-dir
  (expand-file-name "vendor/elpa" user-emacs-directory))

(load-theme 'tau)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(setq mode-line-format '(" τau is installing packages "))
(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Install use-package if not available
(unless  (require 'use-package nil t)
  (package-refresh-contents)
  (package-install 'use-package))


(defun tau/init-esc (frame)
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (and
             (not (terminal-parameter term 'tau/esc-map)))
        (let ((tau/esc-map (lookup-key input-decode-map [?\e])))
          (set-terminal-parameter term 'tau/esc-map tau/esc-map)
          (define-key input-decode-map [?\e]
            `(menu-item "" ,tau/esc-map :filter ,#'tau/esc)))))))

(defun tau/esc (map)
  (if (and (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for tau/esc-quit-delay 0 'no-redisplay))
      (prog1 [escape]
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t)))
    map))

(defun tau/esc-quit ()
  (interactive)
  (message "ESC")
  (if (sit-for tau/esc-quit-wait-delay 0 'no-redisplay)
    (progn
      (tau/quit)
    )
    (progn
      (setq unread-command-events
        (nconc (listify-key-sequence "\C-k\ e") unread-command-events)
      )
    )
  )
)

(defun tau/esc-minibuffer-quit ()
  (interactive)
  (message "ESC")
  (if (sit-for tau/esc-quit-minibuffer-delay 0 'no-redisplay)
    (progn
      (tau/minibuffer-quit)
    )
    (progn
      (setq unread-command-events
        (nconc (listify-key-sequence "\C-k\ e") unread-command-events)
      )
    )
  )
)

(defun tau/mark-quit ()
  (interactive)
  (message "ESC")
  (deactivate-mark)
)

(defun tau/quit ()
  (interactive)
  (multiple-cursors-mode 0)
  (deactivate-mark)
  (setq quit-flat t)
  (message "ESC")
)

(defun tau/minibuffer-quit ()
  (interactive)
  (minibuffer-keyboard-quit)
  (message "ESC")
)

(defun tau-copy-mark (beg end)
  (interactive "r")
  (prog1 (kill-ring-save beg end)
    (setq deactivate-mark nil)))

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))
(require 'undo-tree)

(global-undo-tree-mode)

;; No single key prefix
(setq meta-prefix-char nil)

;; Global ESC control
(global-set-key [escape] 'tau/esc-quit)
(global-set-key "\C-k" nil)

(defvar tau-map (make-sparse-keymap)
  "Keymap for `tau-mode'.")

(define-minor-mode tau-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " tau-mode"
  :keymap tau-map)

(add-to-list 'emulation-mode-map-alists `((tau-mode . ,tau-map)))

(define-globalized-minor-mode global-tau-mode tau-mode tau-mode)

;; Initialize ESC control
(add-hook 'after-make-frame-functions #'tau/init-esc)
(mapc #'tau/init-esc (frame-list))

(setenv "LANG" "en_US.UTF-8")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(set-face-attribute 'variable-pitch t :font "Sans Serif-10" )
(set-face-attribute 'variable-pitch nil :font "Sans Serif-10" )

(require 'tau-editor)
(require 'tau-vendor)
(require 'tau-keys)

(global-tau-mode 1)
