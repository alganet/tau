
(if window-system
    (mouse-wheel-mode t)
  (xterm-mouse-mode t))

(if window-system
    (scroll-bar-mode -1))

(provide 'tau-mouse)
