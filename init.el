;;; package --- Summary:
;;; Commentary:
;;; Code:
(setq inhibit-startup-message t)
(when (memq window-system '(mac ns x))
  (tool-bar-mode -1))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; use-package: a package that makes it easy to install other packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; try: a package that lets you try packages without installing them
(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Org-mode stuff
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook 'turn-on-auto-fill))

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
  (add-hook 'after-init-hook 'global-company-mode))

;; Company-irony
(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

;; Company-c-headers (for c and c++ headers completion)
(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; Python completion + syntax checking
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-checker 'c/c++-clang)))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))
  (add-hook 'c-mode-hook (lambda () (setq flycheck-checker 'c/c++-gcc)))
  (add-hook 'c-mode-hook (lambda () (setq flycheck-gcc-language-standard "gnu99"))))

;; Flycheck-irony
(use-package flycheck-irony
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; Yasnippet snippets
(use-package yasnippet-snippets)

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; Themes
(use-package all-the-icons
  :ensure t)
(use-package badger-theme
  :ensure t
  :config
  (load-theme 'badger t))

;; spaceline
(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (setq spaceline-separator-dir-left '(left . left)
	spaceline-separator-dir-right '(right . right))
  (set-face-attribute 'spaceline-unmodified nil :background "#ed9442")
  (set-face-attribute 'spaceline-modified nil :background "#ef6034"))

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

;; Git
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Make emacs agree with shell on mac
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "CFLAGS")
  (exec-path-from-shell-copy-env "CPPFLAGS")
  (exec-path-from-shell-copy-env "LDFLAGS")
  (exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
  (exec-path-from-shell-copy-env "CPLUS_INCLUDE_PATH")
  (exec-path-from-shell-copy-env "C_INCLUDE_PATH")
  (exec-path-from-shell-copy-env "PKG_CONFIG_PATH")
  (exec-path-from-shell-initialize))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'rust-mode-hook 'rainbow-delimiters-mode))

;; modern-cpp-font-lock for modern c++ highlighting
(use-package modern-cpp-font-lock
  :ensure t)

;; electric-pair
(electric-pair-mode 1)

;; 4 space indent in ccmodes
(setq c-basic-offset 4)

;;quick compile
(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "M-c") 'compile))

;; windmove
(global-set-key (kbd "s-w") 'windmove-up)
(global-set-key (kbd "s-s") 'windmove-down)
(global-set-key (kbd "s-a") 'windmove-left)
(global-set-key (kbd "s-d") 'windmove-right)

;; cmake-mode
(use-package cmake-mode
  :ensure t)

;; auctex
(use-package auctex
  :defer t
  :ensure t
  :config
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    (add-hook 'doc-view-mode-hook
	      (lambda() (auto-revert-mode t)))
    (add-hook 'doc-view-mode-hook
	      (lambda() (setq doc-view-continuous t)))))

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

;; emmet - snippets for html/css
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook #'emmet-mode))

;; fancy-battery - display battery info in modeline
(use-package fancy-battery
  :ensure t
  :config
  (add-hook 'after-init-hook #'fancy-battery-mode))

;; Python
(setq python-shell-interpreter "python3")

;; Misc
(global-hl-line-mode t)
(display-time-mode t)
(setq display-time-default-load-average nil)
(set-face-attribute 'default nil :font "hack-15")
(setq mac-command-modifier 'meta
      mac-right-command-modifier 'super) ; make left command function as alt key
(global-set-key (kbd "s-f") 'forward-word) ; make right command key super
(global-set-key (kbd "s-b") 'backward-word)
(global-set-key (kbd "s-v") 'scroll-down-command)
(when (memq window-system '(mac ns x))
  (scroll-bar-mode -1))
(global-display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda() (display-line-numbers-mode -1)))
(add-hook 'doc-view-mode-hook
	  (lambda() (display-line-numbers-mode -1)))

(column-number-mode t)
(add-hook 'c-mode-common-hook 'auto-fill-mode)
(add-hook 'python-mode-hook 'auto-fill-mode)
(menu-bar-mode -1)
(add-hook
 'eww-mode-hook
 (lambda() (define-key eww-mode-map (kbd "M-c") 'eww-toggle-colors)))
(setq-default fill-column 80)

(add-to-list 'auto-mode-alist '("\\.schedule\\'" . org-mode))

;; open init.el by "M-x init"
(defun init ()
  "Edit the `user-init-file'."
  (interactive)
  ;; (find-file-other-window user-init-file))
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
	)
    (progn
      (shell)
      ))
  )
(global-set-key (kbd "C-`") 'my-toggle-inferior-shell)

;; prevent shell scroll back after clearing screen
(add-hook 'comint-mode-hook
          (defun rm-comint-postoutput-scroll-to-bottom ()
            (remove-hook 'comint-output-filter-functions
                         'comint-postoutput-scroll-to-bottom)))

;; tramp
(setq tramp-verbose 1)

(setq default-directory "~/")

(provide 'init)
;;; init.el ends here
