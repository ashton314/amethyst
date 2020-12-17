;; Entry point for my writer-focused mode
;; Copyright 2020 Ashton Wiersdorf
;;
;; Sizable portions of this code are derived from the elegance theme
;; by Nicolas P. Rougier. Modifications made starting 8 December 2020.
;;
;; -------------------------------------------------------------------
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nice defaults, snarfed from sanity.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold (* 100 1024 1024))
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq-default indent-tabs-mode nil)

;; Don't make new windows pop up. Kind of messes up Magit, but we can
;; work around that.
(setq pop-up-windows nil)

(tool-bar-mode 0)
(tooltip-mode  0)
(scroll-bar-mode 0)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Keep the cursor where you last were when you were editing the file
(save-place-mode 1)

(global-set-key (kbd "C-z") 'undo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic look and feel from elegance.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-font 'default "Input Mono 14")

;; (setq default-frame-alist '((width . 80) (height . 30) (fullscreen . maximized)))

(setq default-frame-alist
      (append (list '(vertical-scroll-bars . nil)
                    '(internal-border-width . 48)
		    '(width  . 80) '(height . 0.9)
		    '(left . 0.5) '(top . 0)
                    '(font . "Input Mono 14"))))

;; (set-frame-parameter (selected-frame)
;;                      'internal-border-width 24)

;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 2)

;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Line cursor and no blink
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 0)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; No Tooltips
(tooltip-mode 0)

;; Paren mode is part of the theme
(show-paren-mode t)

;; No fringe but nice glyphs for truncated and wrapped lines
(fringe-mode '(0 . 0))
(defface fallback '((t :family "Fira Code Light"
                       :inherit 'face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))
(set-display-table-slot standard-display-table 'selective-display
                        (string-to-vector " …"))

;; Mode line (this might be slow because of the "☰" that requires substitution)
;; This line below makes things a bit faster
(set-fontset-font "fontset-default"  '(#x2600 . #x26ff) "Fira Code 16")

(define-key mode-line-major-mode-keymap [header-line]
  (lookup-key mode-line-major-mode-keymap [mode-line]))

(defun mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))
(setq-default mode-line-format
     '((:eval
       (mode-line-render
       (format-mode-line (list
         (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                         'help-echo "Mode(s) menu"
                         'mouse-face 'mode-line-highlight
                         'local-map   mode-line-major-mode-keymap)
         " %b "
         (if (and buffer-file-name (buffer-modified-p))
             (propertize "(modified)" 'face `(:inherit face-faded)))))

       (format-mode-line
        (list
	 (propertize "[ L %l : C %c ]  " 'face `(:inherit face-faded))
	 (propertize
	  (format "%d Words  " (count-words (point-min) (point-max)))
	  'face `(:inherit face-faded))))))))


;; Comment if you want to keep the modeline at the bottom
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format'(""))
              
;; Vertical window divider
(setq window-divider-default-right-width 3)
(setq window-divider-default-places 'right-only)
(window-divider-mode)

;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset a face and make it inherit style."
  (set-face-attribute face nil
   :foreground 'unspecified :background 'unspecified
   :family     'unspecified :slant      'unspecified
   :weight     'unspecified :height     'unspecified
   :underline  'unspecified :overline   'unspecified
   :box        'unspecified :inherit    style))

;; Modeline
(defun set-modeline-faces ()

  ;; Mode line at top
  (set-face 'header-line                                 'face-strong)
  (set-face-attribute 'header-line nil
                                :underline (face-foreground 'default))
  (set-face-attribute 'mode-line nil
                      :height 10
                      :underline (face-foreground 'default)
                      :overline nil
                      :box nil 
                      :foreground (face-background 'default)
                      :background (face-background 'default))
  (set-face 'mode-line-inactive                            'mode-line)
  
  ;; Mode line at bottom
  ;; (set-face 'header-line                                 'face-strong)
  ;; (set-face-attribute 'mode-line nil
  ;;                     :height 1.0
  ;;                     :overline (face-background 'default)
  ;;                     :underline nil
  ;;                     :foreground (face-foreground 'default)
  ;;                     :background (face-background 'face-subtle)
  ;;                     :box `(:line-width 2
  ;;                            :color ,(face-background 'face-subtle)
  ;;                            :style nil))
  ;; (set-face 'mode-line-highlight '(face-popout mode-line))
  ;; (set-face 'mode-line-emphasis  'face-strong)
  ;; (set-face-attribute 'mode-line-buffer-id nil :weight 'regular)
  ;; (set-face-attribute 'mode-line-inactive nil
  ;;                     :height 1.0
  ;;                     :overline (face-background 'default)
  ;;                     :underline nil
  ;;                     :foreground (face-foreground 'face-faded)
  ;;                     :background (face-background 'face-subtle)
  ;;                     :box `(:line-width 2
  ;;                            :color ,(face-background 'face-subtle)
  ;;                            :style nil))


  (set-face-attribute 'cursor nil
                      :background (face-foreground 'default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'mode-line))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground (face-background 'default))
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground (face-background 'default))
  )

;; Buttons
(defun set-button-faces ()
  (set-face-attribute 'custom-button nil
                      :foreground (face-foreground 'face-faded)
                      :background (face-background 'face-subtle)
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-faded)
                             :style nil))
  (set-face-attribute 'custom-button-mouse nil
                      :foreground (face-foreground 'default)
                      ;; :background (face-foreground 'face-faded)
                      :inherit 'custom-button
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-subtle)
                             :style nil))
  (set-face-attribute 'custom-button-pressed nil
                      :foreground (face-background 'default)
                      :background (face-foreground 'face-salient)
                      :inherit 'face-salient
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-salient)
                             :style nil)
                      :inverse-video nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; straight.el setup <<straight.el>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :after 'selectrum
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package markdown-mode)
(use-package org)
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("s-g" . magit-status))

(global-auto-revert-mode)

;; Auto-save stuffs
(setq auto-save-default nil)
(auto-save-visited-mode +1)

;; Writing modes
(mapc (lambda (mode)
       (add-hook mode
                 '(lambda ()
		    (toggle-truncate-lines -1)
		    (turn-on-visual-line-mode)
		    ;; I don't think this is necessary with visual-line-mode turned on
		    ;(toggle-word-wrap t)
		    )))
     '(markdown-mode-hook org-mode-hook text-mode-hook))

(use-package define-word
  :bind
  ("M-#" . define-word))

(selectrum-prescient-mode +1)
(cua-mode)

(defun set-nordic ()
  "Set Nord theme with pretty modeline from elegance"
  (interactive)
  (load-theme 'nord t)
  (set-modeline-faces))

(set-nordic)

(defun scratchpad ()
  (interactive)
  (let ((buff (generate-new-buffer "*Scratchpad*")))
    (set-buffer buff)
    (org-mode)
    (insert "\n*Warning: this is a temporary scratch pad!*\n\nIf you want to save what you wrote after quitting, you need to [[elisp:write-file][save this as a new file]] (keystroke: =[C-x] [C-w]=)\n\n")
    (switch-to-buffer buff)
    (message "Switched to a temporary scratchpad")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-confirm-elisp-link-function nil)

(defvar writer-startup-message
  "


                           *Welcome to Emacs*


   Emacs is the most powerful text editor on the planet. While you
   should be able to start working with Emacs as-is, any effort you
   put into mastering its functions will reward you handsomely. Try
   to get familiar with the *Essential Commands* first, and move on
       to the *Other Commands* once you've got the hang of it.


			    *Quick Links*

[[elisp:find-file][Create a new file]]        [[elisp:find-file-existing][Open an existing file]]        [[elisp:scratchpad][Open a scratchpad]]


*Essential Commands*                             /C: Control, M: alt, S: ⌘/

  Save ........... =[C-x]= =[C-s]=    Help ..................... =[C-h]=
  Save as ........ =[C-x]= =[C-w]=    Cancel ................... =[C-g]=
  Open new file .. =[C-x]= =[C-f]=    Undo ..................... =[C-z]=
  Quit ........... =[C-x]= =[C-c]=    Undo (alternate) ......... =[C-/]=


*Other Commands*                                     /[[info:emacs#Key%2520Bindings][ Other key bindings ]]/

  Search ................. =[C-s]=    Go to line ......... =[M-g]= =[M-g]=
  Replace ................ =[M-%]=    Execute command ............ =[M-x]=
  
  Start of buffer ........ =[M-<]=    End of buffer .............. =[M->]=
  Start of line .......... =[C-a]=    End of line ................ =[C-e]=

  Mark ................. =[C-spc]=    Copy from mark.............. =[M-w]=
  Kill from mark.......... =[C-w]=    Kill from cursor............ =[C-k]=
  Paste .................. =[C-y]=    Paste older ........ =[C-y]= =[M-y]=

*Quick Preferences Settings*                         /[[elisp:(customize-group 'emacs)][ Full preferences ]]/

 [[elisp:menu-set-font][ Select ]]default font                [[elisp:display-line-numbers-mode][ Toggle ]]line numbers
 [[elisp:tool-bar-mode][ Toggle ]]tool bar                    [[elisp:toggle-truncate-lines][ Toggle ]]line wrap
 [[elisp:scroll-bar-mode][ Toggle ]]scroll bar                  [[elisp:blink-cursor-mode][ Toggle ]]blinking cursor
 [[elisp:menu-bar-mode][ Toggle ]]menu bar                     Select cursor:[[elisp:(set-default 'cursor-type  '(hbar . 2))][ HBar ]]|[[elisp:(set-default 'cursor-type  '(bar . 2))][ VBar ]]|[[elisp:(set-default 'cursor-type 'box)][ Box ]]
")

(org-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(enable-recursive-minibuffers t)
 '(find-file-visit-truename t)
 '(frame-resize-pixelwise t)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message writer-startup-message))
