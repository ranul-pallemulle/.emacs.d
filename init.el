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

;; C/C++/ObjC
(setq c-default-style "bsd"
      c-basic-offset 4)
(add-hook 'c-mode-common-hook 'turn-on-auto-fill)
(use-package modern-cpp-font-lock
  :ensure t)
(use-package cmake-mode
  :ensure t)
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

;; Latex
(use-package auctex
  :defer t
  :config
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)))

;; Web
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))
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

;; Org mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (setq fill-column 65)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)


;; Utility
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))
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
  (eval-after-load "auto-revert" '(diminish 'auto-revert-mode))
  (diminish 'magit-auto-revert-mode))
(use-package clang-format
  :ensure t
  :config
  (global-set-key [C-M-tab] 'clang-format-region))
(global-set-key (kbd "ł") 'windmove-up)
(global-set-key (kbd "ß") 'windmove-down)
(global-set-key (kbd "æ") 'windmove-left)
(global-set-key (kbd "ð") 'windmove-right)
(global-set-key (kbd "đ") 'forward-word)
(global-set-key (kbd "”") 'backward-word)
(add-to-list 'load-path "~/.emacs.d/framemove/")
(require 'framemove)
(setq framemove-hook-into-windmove t)


;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-sourcerer t))


;; Miscellaneous
(set-face-attribute 'default nil :font "hack-11")
(set-face-attribute 'region nil :background "#d6972b" :foreground "#ffffff")
(add-to-list 'default-frame-alist '(font . "hack-11"))
(electric-pair-mode t)
(global-display-line-numbers-mode)
(global-set-key (kbd "C-t") (lookup-key global-map (kbd "C-x 5")))
(show-paren-mode 1)
(column-number-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
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
  (interactive)
  (find-file user-init-file))
(defun sched ()
  (interactive)
  (find-file "~/.schedule"))
(defun ag ()
  (interactive)
  (org-agenda))
(defun toggle-inferior-shell ()
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
  (interactive "p")
  (transpose-lines (if (null n) -1 (- n))))
(defun move-line-down (n)
  (interactive "p")
  (transpose-lines (if (null n) 1 n)))
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)
(add-hook 'doc-view-mode-hook (lambda () (auto-revert-mode t)))
(add-hook 'doc-view-mode-hook (lambda () (setq doc-view-continuous t)))
(add-hook 'doc-view-mode-hook 'doc-view-fit-width-to-window)
(add-hook 'doc-view-mode-hook (lambda () (display-line-numbers-mode -1)))

(provide 'init)
;;; init.el ends here
