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

;; Company-rtags - push to company-backends when using rtags
(use-package company-rtags
  :ensure t)

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
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-c/c++-clang-executable "/usr/bin/clang++")))
  (add-hook 'c-mode-hook (lambda () (setq flycheck-checker 'c/c++-gcc)))
  (add-hook 'c-mode-hook (lambda () (setq flycheck-gcc-language-standard "gnu89")))
  (add-hook 'c-mode-hook (lambda () (setq flycheck-c/c++-gcc-executable "/usr/local/Cellar/gcc/9.2.0/bin/gcc-9"))))

;; flycheck-pkg-config - configure flycheck header directories using pkg-config
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

;; RTags (for cmake ide), external daemon using brew
(use-package rtags
  :ensure t
  :config
  (progn
    (setq rtags-path "/usr/local/bin")
    (setq rtags-autostart-diagnostics t)
    (setq rtags-completions-enabled t)
    (rtags-enable-standard-keybindings)
    (push 'company-rtags company-backends)
    (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
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
;; (use-package gruber-darker-theme ;badger-theme cyberpunk-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruber-darker t))
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t))

;; spaceline
(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (setq spaceline-separator-dir-left '(left . left)
	spaceline-separator-dir-right '(right . right))
  (set-face-attribute 'spaceline-unmodified nil :background "#7e5acc")
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
(electric-pair-mode t)

;; 4 space indent in ccmodes
(setq c-basic-offset 4)

;; default C++ compile command for quick compiling
(defun cpp-compile ()
  "Command for quickly compiling a single c++ source file."
  (interactive)
  (setq in (buffer-name))
  (setq out (file-name-sans-extension in))
  (setq quick-compile-command
	(format-spec "g++ -std=c++14 -Wall -Werror -pedantic -o %a %b" (format-spec-make ?a out ?b in)))
  (compile quick-compile-command))

;; default C compile command for quick compiling
(defun c-compile ()
  "Command for quickly compiling a single c source file."
  (interactive)
  (setq in (buffer-name))
  (setq out (file-name-sans-extension in))
  (setq quick-compile-command
	(format-spec "gcc -std=gnu89 -Wall -Werror -pedantic -o %a %b" (format-spec-make ?a out ?b in)))
  (compile quick-compile-command))

(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "M-c") 'cpp-compile))
(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "M-c") 'c-compile))

;; windmove
(global-set-key (kbd "s-w") 'windmove-up)
(global-set-key (kbd "s-s") 'windmove-down)
(global-set-key (kbd "s-a") 'windmove-left)
(global-set-key (kbd "s-d") 'windmove-right)

;; framemove - downloaded manually
(add-to-list 'load-path "~/.emacs.d/framemove/")
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
	      (lambda() (setq doc-view-continuous t)))
    (add-hook 'doc-view-mode-hook
	      'doc-view-fit-width-to-window)))

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

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package flymd
  :ensure t)

;; fancy-battery - display battery info in modeline
(use-package fancy-battery
  :ensure t
  :config
  (add-hook 'after-init-hook #'fancy-battery-mode))

;; Python
(setq python-shell-interpreter "python3")

;; Misc
(display-time-mode t)
(setq display-time-default-load-average nil)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(set-face-attribute 'default nil :font "hack-14")
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

;; prevent shell scroll back after clearing screen
(add-hook 'comint-mode-hook
          (defun rm-comint-postoutput-scroll-to-bottom ()
            (remove-hook 'comint-output-filter-functions
                         'comint-postoutput-scroll-to-bottom)))

(defun import-schedule ()
  "Make a backup of the old schedule file and import schedule from ArchX220 when on the same network."
  (interactive)
  (shell-command-to-string "cp ~/.schedule ~/.schedule.bak")
  (shell-command-to-string "scp -P 200 ranul@ArchX220.local:~/.schedule ~/" )
  (if (get-buffer-window ".schedule")
      (revert-buffer ".schedule")))
(require 'org)
(define-key org-mode-map (kbd "C-c i") (function import-schedule))

(defun export-schedule ()
  "Make a backup of the old schedule file on ArchX220 and copy the new schedule over to it."
  (interactive)
  (shell-command-to-string "scp -P 200 ranul@ArchX220.local:~/.schedule ranul@ArchX220.local:~/.schedule.bak")
  (shell-command-to-string "scp -P 200 ~/.schedule ranul@ArchX220.local:~/"))
(define-key org-mode-map (kbd "C-c e") (function export-schedule))

;; tramp
(setq tramp-verbose 1)

(setq default-directory "~/")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "markdown" t)
 '(package-selected-packages
   '(clang-format flymd markdown-mode flycheck-inline flycheck-pkg-config company-rtags yasnippet-snippets which-key web-mode w3 use-package try treemacs-magit treemacs-icons-dired spaceline-all-the-icons solarized-theme rust-mode rainbow-delimiters org-bullets nyx-theme multiple-cursors modern-cpp-font-lock flycheck-irony fancy-battery exec-path-from-shell emmet-mode elpy doom-themes diminish diff-hl constant-theme company-jedi company-irony company-c-headers cmake-mode badger-theme auctex abyss-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
