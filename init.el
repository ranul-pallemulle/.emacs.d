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
    ("f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" "2a9039b093df61e4517302f40ebaf2d3e95215cb2f9684c8c1a446659ee226b9" "a622aaf6377fe1cd14e4298497b7b2cae2efc9e0ce362dade3a58c16c89e089c" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "0bff60fb779498e69ea705825a2ca1a5497a4fccef93bf3275705c2d27528f2f" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" default)))
 '(package-selected-packages
   (quote
    (rtags cmake-ide cpputils-cmake projectile company-jedi exec-path-from-shell flycheck-irony company-irony company flycheck afternoon-theme magit solarized-theme solarized color-theme auto-complete org-bullets which-key use-package try))))
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
  :config (global-flycheck-mode t))

;; Flycheck-irony
(use-package flycheck-irony
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Themes
;(use-package gruvbox-theme
;  :ensure t
;  :config (load-theme 'gruvbox-dark-soft  t))
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula)
  (doom-themes-org-config))

;; Git porcelain
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

;; RTags (for cmake ide), needs external daemon
(use-package rtags
  :ensure t
  :config
  (progn
    (setq rtags-autostart-diagnostics t)
    (setq rtags-completions-enabled t)
    (rtags-enable-standard-keybindings)
    (require 'company)
    (push 'company-rtags company-backends)
    (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
    (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
    (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
    ;;
    (define-key c-mode-base-map (kbd "M-.") (function rtags-find-symbol-at-point))
    (define-key c-mode-base-map (kbd "M-,") (function rtags-find-references-at-point))))

;; cmake-ide
(use-package cmake-ide
  :ensure t
  :config
  (progn
    (require 'rtags)
    (cmake-ide-setup)))

;; c-mode
(add-hook 'c-mode-common-hook 'electric-pair-mode)
;; python-mode
(add-hook 'python-mode-hook 'electric-pair-mode)

;; Misc
(global-hl-line-mode t)
;;(windmove-default-keybindings) ;; not using these anymore
(global-set-key (kbd "s-w") 'windmove-up)
(global-set-key (kbd "s-s") 'windmove-down)
(global-set-key (kbd "s-a") 'windmove-left)
(global-set-key (kbd "s-d") 'windmove-right)
(set-face-attribute 'default nil :font "Menlo-15")
;;(setq mac-command-modifier 'meta) ; make command function as alt key
(scroll-bar-mode -1)
(global-display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda() (display-line-numbers-mode -1)))
(setq python-shell-interpreter "python3")
(unless (display-graphic-p)
  (menu-bar-mode -1))
(global-set-key (kbd "M-<backspace>") 'delete-forward-char)

;; open init.el by "M-x init"
(defun init ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))



(provide 'init)
;;; init.el ends here
