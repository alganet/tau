
(setq mode-line-format "Initializing theme...")

(unless (require 'aquamacs-tabbar nil t)
  (use-package aquamacs-tabbar
    :ensure t
    :load-path "quelpa/build/aquamacs-tabbar/"
    :quelpa (aquamacs-tabbar :fetcher github :repo "alganet/tabbar")))

(use-package neotree :ensure t)
(use-package spaceline :ensure t)

(defun tau--theme (left second-left &rest additional-segments)
  "Convenience function for the spacemacs and emacs themes."
  (spaceline-compile
    `(,left
      (anzu :priority 95)
      auto-compile
      ,second-left
      (major-mode :priority 79)
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 89)
      (minor-modes :when active
                   :priority 9)
      (mu4e-alert-segment :when active)
      (erc-track :when active)
      (version-control :when active
                       :priority 78)
      (org-pomodoro :when active)
      (org-clock :when active)
      nyan-cat)
    `(which-function
      (python-pyvenv :fallback python-pyenv)
      (purpose :priority 94)
      (battery :when active)
      (selection-info :priority 95)
      input-method
      ((buffer-encoding-abbrev
        point-position
        line-column)
       :separator " · "
       :priority 96)
      (global :when active)
      ,@additional-segments
      (buffer-position :priority 99)
      (hud :priority 99)))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(defun spaceline-tau-theme (&rest additional-segments)
  "Install the modeline used by Spacemacs.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (apply 'tau--theme
         '((persp-name
            workspace-number
            window-number)
           :fallback evil-state
           :face highlight-face
           :priority 100)
         '((buffer-modified buffer-size buffer-id remote-host)
           :priority 98)
          additional-segments))

(require 'spaceline)
(require 'spaceline-config)
;;Valid Values: alternate, arrow, arrow-fade, bar, box, brace,
;;butt, chamfer, contour, curve, rounded, roundstub, wave, zigzag,
;;utf-8."
(setq powerline-default-separator 'zigzag)
(setq powerline-height 20)
(spaceline-toggle-buffer-id-off)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-buffer-modified-off)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-buffer-encoding-on)
(spaceline-toggle-buffer-position-off)
(spaceline-toggle-hud-on)

(spaceline-define-segment hud
  "A HUD that shows which part of the buffer is currently visible."
  (powerline-hud default-face highlight-face)
  :tight t)

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

(eval-after-load 'help-mode
  (lambda ()
    (define-key help-mode-map [menu-bar help-mode] nil)
    (define-key help-mode-map [menu-bar Help-Mode] nil)
    ))

(require 'neotree)
(require 'aquamacs-tabbar)

(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-smart-open t)
(setq neo-show-hidden-files t)


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

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

(add-hook 'emacs-startup-hook
          (lambda () (delete-other-windows)) t)

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

(spaceline-tau-theme)
(spaceline-compile)

(provide 'tau-interface)
