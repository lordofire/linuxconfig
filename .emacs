(package-initialize)

(dolist (mode
	 '(tool-bar-mode                ; No toolbars, more room for text
	   scroll-bar-mode              ; No scroll bars either
	   blink-cursor-mode))          ; The blinking cursor gets old
  (funcall mode 0))

(dolist (mode
	'(abbrev-mode                  ; E.g. sopl -> System.out.println
	  column-number-mode           ; Show column number in mode line
	  delete-selection-mode        ; Replace selected text
	  dirtrack-mode                ; directory tracking in *shell*
	  global-company-mode          ; Auto-completion everywhere
	  global-prettify-symbols-mode ; Greek letters should look greek
	  recentf-mode                 ; Recently opened files
	  show-paren-mode))            ; Highlight matching parentheses
  (funcall mode 1))

(cond ((member "Source Code Pro" (font-family-list))
       (set-face-attribute 'default nil :font "Source Code Pro-13"))
      ((member "Inconsolata" (font-family-list))
              (set-face-attribute 'default nil :font "Inconsolata-14")))

(setq-default c-basic-offset 4)

(global-hl-line-mode 1)
(set-face-attribute 'default nil :background "gray15")
(set-face-attribute 'hl-line nil :foreground nil :background "gray5")

(add-to-list 'load-path "~/elisp")
(require 'go-mode)
