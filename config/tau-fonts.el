(setenv "LANG" "en_US.UTF-8")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(cond
 ((find-font (font-spec :name "Ubuntu Mono"))
  (defvar config-default-font "Ubuntu Mono-13")
  (set-face-font 'variable-pitch "Ubuntu-13"))
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (defvar config-default-font "DejaVu Sans Mono-13")
  (set-face-font 'variable-pitch "DejaVu Sans-13"))
 ((find-font (font-spec :name "Lucida Console"))
  (defvar config-default-font "Lucida Console-13")
  (set-face-font 'variable-pitch "Lucida Sans-13")))

(if window-system
  (set-default-font config-default-font))

(provide 'tau-fonts)
