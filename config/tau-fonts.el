(setenv "LANG" "en_US.UTF-8")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)


;; Use variable width font faces in current buffer
(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Ubuntu Condensed" :height 115))
  (buffer-face-mode))

;; Set default font faces for Info and ERC modes
(add-hook 'neotree-mode-hook 'my-buffer-face-mode-variable)

(set-default-font "Monoid HalfTight-10")
(set-face-attribute 'default t :font "Monoid HalfTight-10" )
(set-face-attribute 'default nil :font "Monoid HalfTight-10" )
(set-face-attribute 'variable-pitch t :font "Ubuntu Condensed-11" )
(set-face-attribute 'variable-pitch nil :font "Ubuntu Condensed-11" )


(provide 'tau-fonts)
