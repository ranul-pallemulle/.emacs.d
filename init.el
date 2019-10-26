;;; package --- Summary:
;;; Commentary:
;;; Code:
(setq inhibit-startup-message t)
(tool-bar-mode -1)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; use-package - easy install/configure packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; org-mode bullet style
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-mode-hook (lambda () (setq fill-column 65)))
  (add-hook 'org-mode-hook 'turn-on-auto-fill))

;; modern-cpp-font-lock for modern c++ highlighting
(use-package modern-cpp-font-lock
  :ensure t)

;; 4 space indent in ccmodes
(setq c-basic-offset 4)

;; auto fill mode on C/C++/ObjC
(add-hook 'c-mode-common-hook 'auto-fill-mode)

;; default C compile command for quick compiling
(require 'format-spec)
(defun c-compile ()
  "Command for quickly compiling a single C source file."
  (interactive)
  (setq in (buffer-name))
  (setq out (file-name-sans-extension in))
  (setq quick-compile-command
	(format-spec
	 "gcc -std=gnu89 -Wall -Werror -Wextra -pedantic -o %a %b"
	 (format-spec-make ?a out ?b in)))
  (compile quick-compile-command))

(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "M-c") 'c-compile))

;; default C++ compile command for quick compiling
(require 'format-spec)
(defun c++-compile ()
  "Command for quickly compiling a single C++ source file."
  (interactive)
  (setq in (buffer-name))
  (setq out (file-name-sans-extension in))
  (setq quick-compile-command
	(format-spec
	 "g++ -std=c++14 -Wall -Werror -Wextra -pedantic -o %a %b"
	 (format-spec-make ?a out ?b in)))
  (compile quick-compile-command))

(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "M-c") 'c++-compile))

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; flymd - view markdown pages live
(use-package flymd
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-sourcerer t))

; spaceline modeline
(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (setq spaceline-separator-dir-left '(left . left)
	spaceline-separator-dir-right '(right . right))
  (set-face-attribute 'spaceline-unmodified nil :background "#7e5acc")
  (set-face-attribute 'spaceline-modified nil :background "#ef6034"))

;; diminish - hide minor modes from modeline
(use-package diminish
  :ensure t
  :config
  (diminish 'yas-minor-mode)
  (diminish 'flycheck-mode)
  (diminish 'company-mode)
  (diminish 'which-key-mode)
  (diminish 'eldoc-mode)
  (diminish 'irony-mode)
  (diminish 'abbrev-mode)
  (diminish 'auto-fill-function)
  (diminish 'auto-revert-mode)
  (diminish 'magit-auto-revert-mode))

;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; electric-pair - automatically close brackets
(electric-pair-mode t)

;; windmove
(windmove-default-keybindings) ;; use shift keys
(global-set-key (kbd "s-w") 'windmove-up)
(global-set-key (kbd "s-f") 'windmove-down)
(global-set-key (kbd "s-a") 'windmove-left)
(global-set-key (kbd "s-d") 'windmove-right)
; make AltGr on Thinkpad + WASD keys do the same
(global-set-key (kbd "ł") (lookup-key global-map (kbd "s-w")))
(global-set-key (kbd "ß") (lookup-key global-map (kbd "s-f")))
(global-set-key (kbd "æ") (lookup-key global-map (kbd "s-a")))
(global-set-key (kbd "ð") (lookup-key global-map (kbd "s-d")))

(global-set-key (kbd "đ") 'forward-word)
(global-set-key (kbd "”") 'backward-word)

(add-to-list 'load-path "~/.emacs.d/framemove/")

;; framemove for windmove functionality across frames.
;; not available on melpa - download manually.
(require 'framemove)
(setq framemove-hook-into-windmove t)

;; clang-format
(use-package clang-format
  :ensure t
  :config
  (global-set-key [C-M-tab] 'clang-format-region))

;; cmake-mode
(use-package cmake-mode
  :ensure t)

;; auctex - edit LaTeX
(use-package auctex
  :defer t
  :ensure t
  :config
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    (add-hook 'doc-view-mode-hook
	      (lamda() (auto-revert-mode t)))
    (add-hook 'doc-view-mode hook
	      (lambda() (setq doc-view-continuous t)))
    (add-hook 'doc-view-mode hook 'doc-view-fit-width-to-window)))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; web-mode for editing html/css and inline/internal javascript/php, etc
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))

;; emmet - snippets for html
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook #'emmet-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Misc
(scroll-bar-mode -1)
(column-number-mode t)
(menu-bar-mode -1)
(global-display-line-numbers-mode)
(show-paren-mode 1)
(setq show-paren-delay 0)
(add-hook 'org-mode-hook (lambda() (display-line-numbers-mode -1)))
(add-hook 'doc-view-mode-hook (lambda() (display-line-numbers-mode -1)))
(global-set-key (kbd "C-t") (lookup-key global-map (kbd "C-x 5")))
(set-face-attribute 'default nil :font "hack-12")
(set-face-attribute 'region nil :background "#d6972b" :foreground "#ffffff")
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; Open schedule in org-mode and add it to org-agenda-files
(setq org-agenda-files (list "~/.schedule"))
(add-to-list 'auto-mode-alist '("\\.schedule\\'" . org-mode))

;; flash the modeline instead of bell
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))
;; prevent audible bell
(setq visible-bell 1)

;; save backup (~) files elsewhere (not in same directory)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)		; copy instead of linking
;; have more than one backup
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; open init.el by "M-x init"
(defun init ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

;; open schedule file by "M-x sched"
(defun sched ()
  "Edit my schedule file."
  (interactive)
  (find-file "~/.schedule"))

;; open org-agenda by "M-x ag"
(defun ag ()
  "Open `org-agenda'."
  (interactive)
  (org-agenda))

;; toggle an inferior shell
(defun my-toggle-inferior-shell ()
  "Toggle the inferior shell."
  (interactive)
  (if (get-buffer-window "*shell*")
      (progn
	(unless (eq (current-buffer) (get-buffer "*shell*"))
	  (other-window 1)
	  )
	(delete-window)
	(jump-to-register ?j)
	)
    (progn
      (window-configuration-to-register ?j)
      (shell)
      )))

(defun my-toggle-inferior-shell-alt ()
  "Toggle the inferior shell by closing and creating a new shell instance."
  (interactive)
  (if (get-buffer-window "*shell*")
      (progn
	(comint-send-eof)
	(with-current-buffer "*shell*" (setq kill-buffer-query-functions nil))
	(kill-buffer "*shell*")
	(jump-to-register ?j))
    (progn
      (window-configuration-to-register ?j)
      (shell)
      )))

;; (global-set-key (kbd "C-`") 'my-toggle-inferior-shell)
(global-set-key (kbd "C-`") 'my-toggle-inferior-shell-alt)

;; prevent shell scroll back after clearing screen (C-l C-l)
(add-hook 'comint-mode-hook
	  (defun rm-comint-postoutput-scroll-to-bottom ()
	    (remove-hook 'comint-output-filter-functions
			 'comint-postoutput-scroll-to-bottom)))

;; Transpose lines
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))
(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))
(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; for server mode, use the same font
(add-to-list 'default-frame-alist '(font . "hack-12"))
(add-to-list 'default-frame-alist '(scroll-bar-mode -1))

;; TRAMP
(setq tramp-verbose 6)

(provide 'init)
;;; init.el ends here
