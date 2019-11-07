;;; package --- Summary:
;;; Commentary:
;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; C/C++/ObjC
(setq c-default-style "bsd"
      c-basic-offset 4)
(add-hook 'c-mode-common-hook 'turn-on-auto-fill)
(use-package modern-cpp-font-lock)
(use-package cmake-mode)
(use-package irony
  :config
  (add-hook 'c-mode-common-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(use-package company-irony
  :config
  (add-to-list 'company-backends 'company-irony))
(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))
(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-checker 'c/c++-clang
					    flycheck-clang-language-standard "c++17")))
  (add-hook 'c-mode-hook (lambda () (setq flycheck-checker 'c/c++-clang
					  flycheck-clang-language-standard "gnu89"))))
(use-package flycheck-inline
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))
(use-package flycheck-irony
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(require 'format-spec)
(defun c-compile ()
  (interactive)
  (setq in (buffer-name))
  (setq out (file-name-sans-extension in))
  (setq quick-compile-command
	(format-spec
	 "gcc -std=gnu89 -Wall -Werror -Wextra -pedantic -o %a %b"
	 (format-spec-make ?a out ?b in)))
  (compile quick-compile-command))
(defun c++-compile ()
  (interactive)
  (setq in (buffer-name))
  (setq out (file-name-sans-extension in))
  (setq quick-compile-command
	(format-spec
	 "g++ -std=c++17 -Wall -Werror -Wextra -pedantic -o %a %b"
	 (format-spec-make ?a out ?b in)))
  (compile quick-compile-command))
(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "M-c") 'c-compile)
  (define-key c++-mode-map (kbd "M-c") 'c++-compile))

;; Python
(use-package elpy
  :init
  (elpy-enable))
(add-hook 'python-mode-hook 'turn-on-auto-fill)

;; Latex
(use-package tex
  :ensure auctex
  :config
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)))
(defun my-latex-compile () (interactive) (compile "make -k"))
(define-key TeX-mode-map (kbd "M-c") 'my-latex-compile)

;; Web
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode)))
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
(use-package emmet-mode
  :config
  (add-hook 'web-mode-hook #'emmet-mode))
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(use-package company-web
  :config
  (add-to-list 'company-backends 'company-web-html))

;; Org mode
(add-hook 'org-mode-hook (lambda () (setq fill-column 65)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)


;; Utility
(use-package magit
  :bind (("C-x g" . magit-status)))
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)
(use-package diminish
  :config
  (diminish 'yas-minor-mode)
  (diminish 'flycheck-mode)
  (diminish 'company-mode)
  (diminish 'which-key-mode)
  (diminish 'eldoc-mode)
  (diminish 'irony-mode)
  (diminish 'abbrev-mode)
  (diminish 'auto-fill-function)
  (eval-after-load "auto-revert" '(diminish 'auto-revert-mode))
  (diminish 'magit-auto-revert-mode))
(use-package clang-format
  :config
  (global-set-key [C-M-tab] 'clang-format-region))
(use-package projectile
  :bind-keymap
  ("s-p" . projectile-command-map)
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/Documents"))
  (projectile-discover-projects-in-search-path))
(use-package which-key
  :config
  (which-key-mode t))
(if (eq system-type 'darwin)
    (progn (global-set-key (kbd "s-w") 'windmove-up)
	   (global-set-key (kbd "s-s") 'windmove-down)
	   (global-set-key (kbd "s-a") 'windmove-left)
	   (global-set-key (kbd "s-d") 'windmove-right)
	   (global-set-key (kbd "s-f") 'forward-word)
	   (global-set-key (kbd "s-b") 'backward-word))
  (progn (global-set-key (kbd "ł") 'windmove-up)
	 (global-set-key (kbd "ß") 'windmove-down)
	 (global-set-key (kbd "æ") 'windmove-left)
	 (global-set-key (kbd "ð") 'windmove-right)
	 (global-set-key (kbd "đ") 'forward-word)
	 (global-set-key (kbd "”") 'backward-word)))
(add-to-list 'load-path "~/.emacs.d/framemove/")
(require 'framemove)
(setq framemove-hook-into-windmove t)


;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-laserwave t))


;; Miscellaneous
(if (eq system-type 'darwin)
    (progn (setq mac-option-modifier 'meta)
	   (setq mac-right-command-modifier 'super)))
(if (eq system-type 'darwin) (setq var-font "monaco-14") (setq var-font "hack-11"))
(set-face-attribute 'default nil :font var-font)
(set-face-attribute 'region nil :background "#d6972b" :foreground "#ffffff")
(add-to-list 'default-frame-alist `(font . ,var-font))
(electric-pair-mode t)
(global-display-line-numbers-mode)
(global-set-key (kbd "C-t") (lookup-key global-map (kbd "C-x 5")))
(show-paren-mode 1)
(column-number-mode 1)
(delete-selection-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(unless (eq system-type 'darwin) (menu-bar-mode -1))
(add-to-list 'default-frame-alist '(scroll-bar-mode -1))
(setq ring-bell-function (lambda ()
			   (let ((orig-fg (face-foreground 'mode-line)))
			     (set-face-foreground 'mode-line "#F2804F")
			     (run-with-idle-timer 0.1 nil
						  (lambda (fg) (set-face-foreground 'mode-line fg))
						  orig-fg))))
(setq visible-bell 1)
(setq show-paren-delay 0)
(setq inhibit-startup-message t)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq org-agenda-files (list "~/.schedule"))
(add-to-list 'auto-mode-alist '("\\.schedule\\'" . org-mode))
(defun init ()
  "Open init.el."
  (interactive)
  (find-file user-init-file))
(defun sched ()
  "Open ~/.schedule."
  (interactive)
  (find-file "~/.schedule"))
(defun ag ()
  "Open 'org-mode' agenda."
  (interactive)
  (org-agenda))
(defun toggle-inferior-shell ()
  "Open inferior shell in current directory."
  (interactive)
  (if (get-buffer-window "*shell*")
      (progn
	(comint-send-eof)
	(with-current-buffer "*shell*" (setq kill-buffer-query-functions nil))
	(kill-buffer "*shell*")
	(jump-to-register ?j))
    (progn
      (window-configuration-to-register ?j)
      (shell))))
(global-set-key (kbd "C-`") 'toggle-inferior-shell)
(add-hook 'comint-mode-hook
	  (defun rm-comint-postoutput-scroll-to-bottom ()
	    (remove-hook 'comint-output-filter-functions
			 'comint-postoutput-scroll-to-bottom)))
(defun transpose-lines (n)
  "Move current line N lines down."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    (forward-line -1)
    (forward-char col)))
(defun move-line-up (n)
  "Wrapper around 'transpose-lines' for moving current line N lines up."
  (interactive "p")
  (transpose-lines (if (null n) -1 (- n))))
(defun move-line-down (n)
  "Wrapper around 'transpose-lines' for moving current line N lines down."
  (interactive "p")
  (transpose-lines (if (null n) 1 n)))
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)
(defun newline-from-anywhere ()
  "Create a new line and go to it regardless of current cursor column."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "M-RET") 'newline-from-anywhere)
(defun copy-line-down ()
  "Replicate current line one line below while maintaining cursor column."
  (interactive)
  (let ((saved-col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (move-to-column saved-col)))
(global-set-key (kbd "M-<down>") 'copy-line-down)
(add-hook 'doc-view-mode-hook (lambda ()
				(progn
				  (display-line-numbers-mode -1)
				  (auto-revert-mode t))))
;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
      (lambda (status code msg)
	;; If M-x compile exists with a 0
	(when (and (eq status 'exit) (zerop code))
	  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
  	  (bury-buffer "*compilation*")
  	  ;; and return to whatever were looking at before
  	  (replace-buffer-in-windows "*compilation*"))
	;; Always return the anticipated result of compilation-exit-message-function
  	(cons msg code)))

(provide 'init)
;;; init.el ends here
