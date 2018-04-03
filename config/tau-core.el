
(eval-when-compile
  ;; Install use-package if not available
  (unless  (require 'use-package nil t)
    (package-refresh-contents)
    (package-install 'use-package)))

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

(defun tau/tab ()
    (interactive)
  (if (use-region-p) (shift-right) (ac-complete)))

(defun tau/backtab ()
    (interactive)
  (if (use-region-p) (shift-left)))

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


    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

    (setq scroll-step 1) ;; keyboard scroll one line at a time

(require 'tau-editor)
(require 'tau-vendor)
(require 'tau-keys)
(load-theme 'nord)
(redisplay t)
(global-tau-mode 1)
(provide 'tau-core)
