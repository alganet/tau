
(if window-system
  (mouse-wheel-mode t)
  (xterm-mouse-mode t))

(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

(setq mouse-drag-copy-region nil)

(provide 'tau-mouse)
