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

;; Nice defaults, snarfed from sanity.el
(setq gc-cons-threshold (* 100 1024 1024))
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq-default indent-tabs-mode nil)
(setq pop-up-windows nil)
(tool-bar-mode 0)
(tooltip-mode  0)
(scroll-bar-mode 0)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(save-place-mode 1)

(global-set-key (kbd "C-z") 'undo)

(load-file "elegance.el")

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

(setq default-frame-alist '((width . 87) (height . 60) (vertical-scroll-bars)))

(defvar writer-startup-message
  "


                           _Welcome to Emacs_



                             *Quick Links*

[[elisp:find-file][Create a new file]]        [[elisp:find-file-existing][Open an existing file]]        [[elisp:scratchpad][Open a scratchpad]]



*Essential commands*                             /C: Control, M: alt, S: ⌘/

  Save ............... =[C-x]= =[C-s]=    Help ..................... =[C-h]=
  Save as ............ =[C-x]= =[C-w]=    Cancel ................... =[C-g]=
  Open a new file .... =[C-x]= =[C-f]=    Undo ..................... =[C-/]=
  Browse directory ..... =[C-x]= =[d]=    Quit ............... =[C-x]= =[C-c]= 


*Other commands*                                     /[[info:emacs#Key%2520Bindings][ Other key bindings ]]/

  Search ................... =[C-s]=    Go to line ......... =[M-g]= =[M-g]=
  Replace .................. =[M-%]=    Execute .................. =[M-x]=
  
  Start of buffer .......... =[M-<]=    End of buffer ............ =[M->]=
  Start of line ............ =[C-a]=    End of line .............. =[C-e]=

  Mark ................... =[C-spc]=    Copy from mark............ =[M-w]=
  Kill from mark............ =[C-w]=    Kill from cursor.......... =[C-k]=
  Paste .................... =[C-y]=    Paste older ........ =[C-y]= =[M-y]=

*Quick preferences*                                    /[[elisp:(customize-group%20'emacs)][ Full preferences ]]/

 [[elisp:menu-set-font][ Select ]]default font                [[elisp:display-line-numbers-mode][ Toggle ]]line numbers
 [[elisp:tool-bar-mode][ Toggle ]]tool bar                    [[elisp:toggle-truncate-lines][ Toggle ]]line wrap
 [[elisp:scroll-bar-mode][ Toggle ]]scroll bar                  [[elisp:blink-cursor-mode][ Toggle ]]blinking cursor
 [[elisp:menu-bar-mode][ Toggle ]]menu bar                     Select cursor:[[elisp:(set-default%20'cursor-type%20%20'(hbar%20.%202))][ HBar ]]|[[elisp:(set-default%20'cursor-type%20%20'(bar%20.%202))][ VBar ]]|[[elisp:(set-default%20'cursor-type%20'box)][ Box ]]
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
