
(if window-system
    (mouse-wheel-mode t)
  (xterm-mouse-mode t))

(if window-system
    (scroll-bar-mode -1))

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq mouse-drag-copy-region nil)

(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 10)
  )


(setq scroll-margin 3
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

(provide 'tau-mouse)
