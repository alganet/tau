;; Tau init.el
;;
;; Configures the Tau editor on top of emacs
;;

;; Initialize packaging system
(package-initialize)

;; Performance
(setq gc-cons-threshold 20000000)

;; Define user configuration directory
(setq user-config-directory
  (expand-file-name "config/" user-emacs-directory))

;; Define user configuration file
(setq user-config-file
  (expand-file-name "config.el" user-emacs-directory))

;; Create user configuration directory if not exists
(unless (file-exists-p user-config-directory)
  (make-directory user-config-directory))

;; Load packages from user configuration directory
(add-to-list 'load-path user-config-directory)

;; Setup standard package archive sources
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")))

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

;; Loads User Configuration
(eval-when-compile (load-file user-config-file))

;; Tau-Fu Time!!!

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
