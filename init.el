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

;; try - try packages temporarily
(use-package try
  :ensure t)

;; which-key - buffer with keybinding completion options
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; org-mode bullet style
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(add-hook 'org-mode-hook '(lambda () (setq fill-column 65)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; Make windmove work in org-mode:
(setq org-disputed-keys '(([(shift up)] . [(meta up)])
                         ([(shift down)] . [(meta down)])
                         ([(shift left)] . [(meta left)])
                         ([(shift right)] . [(meta right)])
                         ([(meta return)] . [(control meta return)])
                         ([(control shift right)] . [(meta shift +)])
                         ([(control shift left)] . [(meta shift -)])))
(setq org-replace-disputed-keys t)

;; Irony mode
(use-package irony
  :ensure t
  :config
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; Company
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
;prevent auto lowercasing of completions
  (add-hook 'org-mode-hook
	    (lambda () (setq-local company-dabbrev-downcase nil))))

;; Company-irony
(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

;; Company-rtags - push to company-backends when using rtags
(use-package company-rtags
  :ensure t)

;; Company-c-headers (for c and c++ headers completion)
(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; Python autocompletion
(use-package company-jedi
  :ensure t
  :config
  (defun jedi-python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'jedi-python-mode-hook))

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-checker 'c/c++-gcc)))
  (add-hook 'c++-mode-hook (lambda ()
			     (setq flycheck-gcc-language-standard "c++11")))
  (add-hook 'c++-mode-hook (lambda ()
			     (setq flycheck-c/c++-gcc-executable "/usr/bin/g++")))
  (add-hook 'c-mode-hook (lambda () (setq flycheck-checker 'c/c++-gcc)))
  (add-hook 'c-mode-hook (lambda ()
			   (setq flycheck-gcc-language-standard "gnu99")))
  (add-hook 'c-mode-hook (lambda ()
			   (setq flycheck-c/c++-gcc-executable "/usr/bin/gcc"))))

;; flycheck-pkg-config - configure flycheck using pkg-config header directories
(use-package flycheck-pkg-config
  :ensure t)

;; flycheck-inline - show error messages inline
(use-package flycheck-inline
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;; Flycheck-irony
(use-package flycheck-irony
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; flymd - view markdown pages live
(use-package flymd
  :ensure t)

;; all-the-icons
(use-package all-the-icons
  :ensure t)

;; Theme
;; List of favourites (ranked top to bottom):
;  - dracula-theme
;  - cyberpunk-theme
;  - badger-theme
;  - nyx-theme
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

; spaceline modeline
(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (setq spaceline-separator-dir-left '(left . left)
	spaceline-separator-dir-right '(right . right))
  (set-face-attribute 'spaceline-unmodified nil :background "#ed9442")
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
  (diminish 'magit-auto-revert-mode)
  )

;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; diff-hl - for highlighting uncommited changes
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode t))

;; exec-path-from-shell - import environment variables from shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "C_INCLUDE_PATH")
  (exec-path-from-shell-copy-env "CPLUS_INCLUDE_PATH")
  (exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
  (exec-path-from-shell-copy-env "PKG_CONFIG_PATH")
  (exec-path-from-shell-initialize))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; RTags (for cmake ide), needs external daemon
(use-package rtags
  :ensure t
  :config
  (progn
    (setq rtags-path "/usr/local/bin/")
    (setq rtags-autostart-diagnostics t)
    (setq rtags-completions-enabled t)
    (rtags-enable-standard-keybindings)
    (push 'company-rtags company-backends)
    (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
    (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
    (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
    (define-key c-mode-base-map (kbd "M-.")
      (function rtags-find-symbol-at-point))
    (define-key c-mode-base-map (kbd "M-,")
      (function rtags-find-references-at-point))
    (define-key c-mode-base-map (kbd "M-(")
      (function rtags-location-stack-back))
    (define-key c-mode-base-map (kbd "M-)")
      (function rtags-find-symbol-at-point))
    (define-key c-mode-base-map (kbd "<C-tab>")
      (function company-complete))))

;; Flycheck-rtags
(use-package flycheck-rtags
 :ensure t)

;; cmake-ide
(use-package cmake-ide
 :ensure t
 :config
 (progn
   (require 'rtags)
   (cmake-ide-setup)))

;; modern-cpp-font-lock for modern c++ highlighting
(use-package modern-cpp-font-lock
  :ensure t)

;; electric-pair - automatically close brackets
(add-hook 'c-mode-hook 'electric-pair-mode)
(add-hook 'c++-mode-hook 'electric-pair-mode)
(add-hook 'python-mode-hook 'electric-pair-mode)
(add-hook 'rust-mode-hook 'electric-pair-mode)
(add-hook 'web-mode-hook 'electric-pair-mode)

;; 4 space indent in ccmodes
(setq c-basic-offset 4)

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

;; multi-term
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/usr/bin/bash"))

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

;; fancy-battery - display battery status in modeline
;; (use-package fancy-battery
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook #'fancy-battery-mode))

;; stickyfunc-enhance - show function name that was scrolled past - annoying random freezes
;; (use-package stickyfunc-enhance
;;   :ensure t
;;   :config
;;   (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;   (semantic-mode 1))

;; Misc
(set-face-attribute 'default nil :font "hack-11")
(set-face-attribute 'region nil :background "#d6972b" :foreground "#ffffff")
(scroll-bar-mode -1)
(global-display-line-numbers-mode)
(display-time-mode t)
(setq display-time-default-load-average nil)
(add-hook 'org-mode-hook (lambda() (display-line-numbers-mode -1)))
(add-hook 'doc-view-mode-hook (lambda() (display-line-numbers-mode -1)))
(column-number-mode t)
(add-hook 'c-mode-common-hook 'auto-fill-mode)
(menu-bar-mode -1)
(add-hook 'eww-mode-hook
	  (lambda()(define-key eww-mode-map (kbd "M-c") 'eww-toggle-colors)))
(global-set-key (kbd "C-t") (lookup-key global-map (kbd "C-x 5")))
(global-set-key (kbd "s-u") 'revert-buffer)
(show-paren-mode 1)
(setq show-paren-delay 0)		;remove the default delay
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
	;(message "Hiding shell")
	(unless (eq (current-buffer) (get-buffer "*shell*"))
	  (other-window 1)
	  )
	(delete-window)
	(jump-to-register ?j)
	)
    (progn
      (window-configuration-to-register ?j)
      (shell)
      ))
  )
(global-set-key (kbd "C-`") 'my-toggle-inferior-shell)

;; quick compile
(with-eval-after-load 'ccmode
(define-key c-mode-base-map (kbd "M-c") 'compile))

;; prevent shell scroll back after clearing screen (C-l C-l)
(add-hook 'comint-mode-hook
	  (defun rm-comint-postoutput-scroll-to-bottom ()
	    (remove-hook 'comint-output-filter-functions
			 'comint-postoutput-scroll-to-bottom)))

;; for server mode, use the same font
(add-to-list 'default-frame-alist '(font . "hack-11"))
(add-to-list 'default-frame-alist '(scroll-bar-mode -1))

;; TRAMP
(setq tramp-verbose 6)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (naysayer-theme company-rtags yasnippet-snippets which-key web-mode use-package try treemacs-projectile treemacs-magit treemacs-icons-dired stickyfunc-enhance spaceline-all-the-icons rust-mode rainbow-delimiters org-bullets multiple-cursors multi-term monokai-theme modern-cpp-font-lock gruvbox-theme flymd flycheck-rtags flycheck-pkg-config flycheck-irony flycheck-inline fancy-battery exotica-theme exec-path-from-shell emmet-mode doom-themes diminish diff-hl counsel-etags company-jedi company-irony company-c-headers cmake-mode cmake-ide clang-format badger-theme auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
