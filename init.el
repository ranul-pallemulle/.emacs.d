;;; package --- Summary:
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "43b219a31db8fddfdc8fdbfdbd97e3d64c09c1c9fdd5dff83f3ffc2ddb8f0ba0" "04589c18c2087cd6f12c01807eed0bdaa63983787025c209b89c779c61c3a4c4" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default)))
 '(package-selected-packages
   (quote
    (clang-format rtags latex-preview-pane auctex abyss-theme clues-theme oceanic-theme atom-one-dark-theme rainbow-delimiters yasnippet-snippets spaceline-all-the-icons which-key use-package try spaceline smart-mode-line-powerline-theme smart-mode-line-atom-one-dark-theme projectile org-bullets monokai-theme modern-cpp-font-lock magit gruvbox-theme flycheck-rtags flycheck-irony exotica-theme exec-path-from-shell doom-themes cpputils-cmake counsel-etags company-jedi company-irony company-c-headers cmake-ide))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(setq inhibit-startup-message t)
(tool-bar-mode -1)

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
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; Make windmove work in org-mode:
;(setq org-disputed-keys '(([(shift up)] . [(meta p)])
;                          ([(shift down)] . [(meta n)])
;                          ([(shift left)] . [(meta -)])
;                          ([(shift right)] . [(meta +)])
;                          ([(meta return)] . [(control meta return)])
;                          ([(control shift right)] . [(meta shift +)])
;                          ([(control shift left)] . [(meta shift -)])))
					;(setq org-replace-disputed-keys t)

;; Auto complete
;;(use-package auto-complete
;;  :ensure t
;;  :init
;;  (progn
;;    (ac-config-default)
;;    (global-auto-complete-mode t)
;;    ))

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
  (add-hook 'org-mode-hook (lambda () (setq-local company-dabbrev-downcase nil)))) ;prevent auto lowercasing of completions

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

;; Python autocompletion
(use-package company-jedi
  :ensure t
  :config
  (defun jedi-python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'jedi-python-mode-hook))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-args "-O2"))))

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
(use-package yasnippet-snippets)

;; Themes
;; (use-package gruvbox-theme
;;  :ensure t
;;  :config (load-theme 'gruvbox-dark-soft  t))
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-one)
;;   (doom-themes-org-config))
;; (use-package exotica-theme
;;  :ensure t
;;  :config (load-theme 'exotica t))

;; smart-mode-line (now using spaceline instead)
;(use-package smart-mode-line
;  :ensure t
;  :config
;  (sml/setup)
;  (sml/apply-theme 'respectful))
;(set-face-attribute 'mode-line-buffer-id nil :background "#65d5f7" :foreground "black")

; all-the-icons (for spaceline)
;(use-package all-the-icons)

; spaceline
(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified))
;(use-package spaceline-all-the-icons
;  :after spaceline
;  :config
;  (spaceline-all-the-icons-theme))

(setq ;spaceline-all-the-icons-separator-type 'wave
      spaceline-separator-dir-left '(left . left)
      spaceline-separator-dir-right '(right . right)
      spaceline-minor-modes-separator " > "
      )

;(setq spaceline-all-the-icons-icon-set-flycheck-slim 'dots
;      spaceline-all-the-icons-icon-set-git-ahead 'commit
;      spaceline-all-the-icons-flycheck-alternate t
;      spaceline-all-the-icons-highlight-file-name t
;      spaceline-highlight-face-func 'spaceline-highlight-face-modified)

(set-face-attribute 'spaceline-unmodified nil :background "#ed9442") ; LightSkyBlue
(set-face-attribute 'spaceline-modified nil :background "#ef6034") ; #f7e165

;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Make emacs agree with shell on mac
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
    ;;
    (define-key c-mode-base-map (kbd "M-.") (function rtags-find-symbol-at-point))
    (define-key c-mode-base-map (kbd "M-,") (function rtags-find-references-at-point))
    (define-key c-mode-base-map (kbd "M-*") (function rtags-location-stack-back))
    (define-key c-mode-base-map (kbd "M-(") (function rtags-location-stack-forward))))

;; cmake-ide
;(use-package cmake-ide
;  :ensure t
;  :config
;  (progn
;    (require 'rtags)
;    (cmake-ide-setup)
;    (setq cmake-ide-build-pool-use-persistent-naming t)
;    (setq cmake-ide-flags-c++ (append '("-std=c++11")))))

;; Flycheck-rtags
;(use-package flycheck-rtags
;  :ensure t)

;(require 'flycheck-rtags)
;(defun my-flycheck-rtags-setup ()
;  "RTags setup for c/c++/java."
;  (flycheck-select-checker 'rtags)
;  (setq-local flycheck-highlighting-mode nil)
					;  (setq-local flycheck-check-syntax-automatically nil))
;(add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
;(add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)

;; counsel-etags with universal-ctags as backend (replacing rtags)
;(use-package counsel-etags
;  :ensure t
;  :config
;  (require 'cc-mode)
;  (define-key c-mode-base-map (kbd "C-c r") 'counsel-etags-find-tag-at-point))

;; modern-cpp-font-lock for modern c++ highlighting
(use-package modern-cpp-font-lock
  :ensure t)

;; electric-pair
(add-hook 'c-mode-hook 'electric-pair-mode)
(add-hook 'c++-mode-hook 'electric-pair-mode)
(add-hook 'python-mode-hook 'electric-pair-mode)

;; Use linux kernel style for c major modes
;(setq c-default-style '((java-mode . "java")
;                        (awk-mode . "awk")
;                        (other . "linux")))
;; 4 space indent in ccmodes
(setq c-basic-offset 4)

;; windmove
;;(windmove-default-keybindings) ;; not using these anymore
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

;; clang-format
(use-package clang-format
  :ensure t
  :config
  (global-set-key [C-M-tab] 'clang-format-region))

;; Python
;(setq python-shell-interpreter "python")

;; Misc
(global-hl-line-mode t)
(set-face-attribute 'default nil :font "inconsolata-15") ; Menlo-15 is nice too
;;(setq mac-command-modifier 'meta) ; make command function as alt key
(scroll-bar-mode -1)
(global-display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda() (display-line-numbers-mode -1)))
(column-number-mode t)
(add-hook 'c-mode-common-hook 'auto-fill-mode)
(menu-bar-mode -1)
(global-set-key (kbd "M-<backspace>") 'delete-forward-char)
(add-hook 'eww-mode-hook (lambda()(
				   define-key eww-mode-map (kbd "M-c") 'eww-toggle-colors)))
(global-set-key (kbd "C-t") (lookup-key global-map (kbd "C-x 5")))

;; open init.el by "M-x init"
(defun init ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

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
	)
    (progn
      ;(message "Showing shell")
      (shell)
      ))
  )
(global-set-key (kbd "C-`") 'my-toggle-inferior-shell)

(provide 'init)
;;; init.el ends here
