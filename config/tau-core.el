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
  (multiple-cursors-mode 0)
  (deactivate-mark)
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
  (multiple-cursors-mode 0)
  (deactivate-mark)
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

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun duplicate-current-line-or-region (arg)
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun remove-current-line-or-region (arg)
  (interactive "p")
  (let (beg end)
    (if (and mark-active (> (point) (mark))) (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (delete-region beg end)
    (kill-whole-line)
  ))

(defun move-region-down (arg)
   (interactive "*p")
   (move-text-internal arg))

(defun move-region-up (arg)
   (interactive "*p")
   (move-text-internal (- arg)))

(defun smart-beginning-of-line ()
  "Move point to `beginning-of-line'. If repeat command it cycle
position between `back-to-indentation' and `beginning-of-line'."
  (interactive "^")
  (if (and (eq last-command 'my--smart-beginning-of-line)
           (= (line-beginning-position) (point)))
      (back-to-indentation)
    (beginning-of-line)))

(defun smart-end-of-line ()
  "Move point to `end-of-line'. If repeat command it cycle
position between last non-whitespace and `end-of-line'."
  (interactive "^")
  (if (and (eq last-command 'my--smart-end-of-line)
           (= (line-end-position) (point)))
      (skip-syntax-backward " " (line-beginning-position))
    (end-of-line)))

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

(require 'eshell)
(require 'tau-editor)
(require 'tau-vendor)
(require 'tau-keys)
(projectile-global-mode 1)
(global-tau-mode 1)
(load-theme 'nord)
(redisplay t)
(provide 'tau-core)
