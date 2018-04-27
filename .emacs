;; USAGE: Copy this file to your home directory and rename it as .emacs.                               

;; Configurations marked 'REQUIRED' follow the coding guidelines put forth in                          
;; EDCS-870189 and should not be altered or removed. Other configurations are                          
;; optional and can be commented in or out based on your preferences.                                  

;; REQUIRED: Set the fill column to 79 (80 columns total):                                             

;; Added by Package.el.  This must come before configurations of                                       
;; installed packages.  Don't delete this line.  If you don't want it,                                 
;; just comment it out by adding a semicolon to the start of the line.                                 
;; You may delete these explanatory comments.                                                          
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(setq default-fill-column 79)

;; Set delete keys to "forward" delete:                                                                
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Turn on font lock mode for color coding:                                                            
(global-font-lock-mode t)

;; Create you color coding scheme -- change colors to your preference:                                 
(add-hook 'font-lock-mode-hook
          '(lambda ()
         (set-face-foreground 'font-lock-constant-face "Red")
         (set-face-foreground 'font-lock-builtin-face "Sienna")
         (set-face-foreground 'font-lock-comment-face "DarkGreen")
         (set-face-foreground 'font-lock-string-face "Yellow")
         (set-face-foreground 'font-lock-keyword-face "BlueViolet")
         (set-face-foreground 'font-lock-function-name-face "Firebrick")
         (set-face-foreground 'font-lock-variable-name-face "OrangeRed")
         (set-face-foreground 'font-lock-type-face "MediumBlue")
         ))

;; Create your c coding style:                                                                         
(defvar my-c-style
   '(
; REQUIRED: Basic indentation offset:                                                                  
     (c-basic-offset . 2)
; REQUIRED: Extra offset for line which contains only the start of a comment:                          
     (c-comment-only-line-offset . 0)
; REQUIRED: Set the fill column to 79 (80 columns total):                                              
     (fill-column . 79)
; REQUIRED: Allow for multi-line comments:                                                             
     (comment-multi-line . t)
; REQUIRED: Tabs insert spaces instead of tab character:                                               
     (indent-tabs-mode . nil)
; REQUIRED: Column to indent right-margin comments to:                                                 
     (comment-column . 0)
; REQUIRED: Set indentation offsets for various c elements:                                            
     (c-offsets-alist . ((knr-argdecl-intro . 0)
                         (knr-argdecl . 0)
                        (cpp-macro-cont . 4)
                         (label . 0)
                         (statement-cont . +)
                         (substatement-open . 0))))
; Show column number in minibuffer:                                                                    
;     (column-number-mode . t)                                                                         
; Show indentation info in minibuffer:                                                                 
;     (c-echo-syntactic-information-p . t)                                                             
; Show status messages for buffer fontification:                                                       
;     (font-lock-verbose . t)                                                                          
; Allow auto-fill:                                                                                     
;     (auto-fill-mode . t)                                                                             
; Allow tabs in the middle of a line:                                                                  
;     (c-tab-always-indent . nil)                                                                      
; Automatically create newline after semi-colon, brackets, etc:                                        
;     (c-auto-newline . t)                                                                             
; Automatically delete all white space on delete key:                                                  
;     (c-hungry-delete-key . t)                                                                        
; Level of color coding:                                                                               
;     (font-lock-maximum-decoration . 3)                                                               
   "my c-style for cc-mode")

; Invoke your c style:                                                                                 
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-add-style "my-style" my-c-style)
             (c-set-style "my-style")))

;; custom-set-variables                                                                                
(custom-set-variables
 ;; custom-set-variables was added by Custom.                                                          
 ;; If you edit it by hand, you could mess it up, so be careful.                                       
 ;; Your init file should contain only one such instance.                                              
 ;; If there is more than one, they won't work right.                                                  
 '(c-font-lock-extra-types
   (quote
    ("FILE" "\\sw+_t" "uchar" "uint" "ulong" "ushort" "uint8" "uint16" "uint32" "uint64" "int8" "int16\
" "int32" "int64" "bool" "boolean" "STATUS" "MSG_Q_ID" "SEM_ID" "BOOL")))
 '(case-fold-search t)
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(global-font-lock-mode t nil (font-lock))
 '(package-selected-packages (quote (magit)))
 '(save-place t)
 '(show-paren-mode t nil (paren))
 '(uniquify-buffer-name-style nil nil (uniquify)))

;; custom-set-faces                                                                                    
;;(custom-set-faces                                                                                    
;; '(default ((t (:stipple nil :background "orange" :foreground "black" :inverse-video nil :box nil :s\
trike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal \
:family "misc-fixed")))))                                                                              

(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)

;; (setq cscope-do-not-update-database t)                                                              
;; (require 'xcscope)                                                                                  

; use % on the backward parenthsis to go to its matching front parenthsis                              
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

; comment out the #if 0 code block                                                                     
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun my-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(custom-set-faces
 ;; custom-set-faces was added by Custom.                                                              
 ;; If you edit it by hand, you could mess it up, so be careful.                                       
 ;; Your init file should contain only one such instance.                                              
 ;; If there is more than one, they won't work right.                                                  
 )


;; Customizing colors used in diff mode                                                                
(defun custom-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute
   'diff-added nil :foreground "green")
  (set-face-attribute
   'diff-removed nil :foreground "red")
  (set-face-attribute
   'diff-changed nil :foreground "purple"))
(eval-after-load "diff-mode" '(custom-diff-colors))

(global-set-key "\C-x\C-b" 'buffer-menu)
