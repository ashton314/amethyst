;; Entry point for my writer-focused mode
;; Ashton Wiersdorf

(load-file "sanity.el")
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

;; Initial frame
(setq default-frame-alist '((width . 87) (height . 60) (vertical-scroll-bars)))

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
