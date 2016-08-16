;; Tau init.el
;;
;; Configures the Tau editor on top of emacs
;;

;; Initialize packaging system
(package-initialize)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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
      '(("melpa"        . "https://elpa.zilongshanren.com/melpa/")
        ("melpa-stable" . "https://elpa.zilongshanren.com/melpa-stable/")
        ("gnu"          . "https://elpa.gnu.org/packages/")))

(setq ssl-program-name "gnutls-cli"
      ssl-program-arguments '("--insecure" "-p" service host)
      ssl-certificate-verification-policy 1)

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

(setq custom-file "~/.emacs.d/custom.el")

;; Tau-Fu Time!!!

(provide 'init)
