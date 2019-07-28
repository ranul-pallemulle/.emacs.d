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
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-checker 'c/c++-gcc)))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
  (add-hook 'c-mode-hook (lambda () (setq flycheck-checker 'c/c++-gcc)))
  (add-hook 'c-mode-hook (lambda () (setq flycheck-gcc-language-standard "gnu99")))
  )

(use-package flycheck-pkg-config
  :ensure t)

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

(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; markdown
(use-package flymd
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package badger-theme ; nyx-theme
  :ensure t
  :config
  (load-theme 'badger t))

; spaceline
(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (setq spaceline-separator-dir-left '(left . left)
	spaceline-separator-dir-right '(right . right))
  (set-face-attribute 'spaceline-unmodified nil :background "#ed9442") ; LightSkyBlue
  (set-face-attribute 'spaceline-modified nil :background "#ef6034")) ; #f7e165

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

;; Make emacs agree with shell on mac
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
;; (use-package rtags
;;   :ensure t
;;   :config
;;   (progn
;;     (setq rtags-path "/usr/local/bin/")
;;     (setq rtags-autostart-diagnostics t)
;;     (setq rtags-completions-enabled t)
;;     (rtags-enable-standard-keybindings)
;;     (push 'company-rtags company-backends)
;;     (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;;     (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
;;     (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
;;     (define-key c-mode-base-map (kbd "M-.") (function rtags-find-symbol-at-point))
;;     (define-key c-mode-base-map (kbd "M-,") (function rtags-find-references-at-point))
;;     (define-key c-mode-base-map (kbd "M-(") (function rtags-location-stack-back))
;;     (define-key c-mode-base-map (kbd "M-)") (function rtags-find-symbol-at-point))
;;     (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))))

;; ;; Flycheck-rtags
;; (use-package flycheck-rtags
;;  :ensure t)

;; ;; cmake-ide
;; (use-package cmake-ide
;;  :ensure t
;;  :config
;;  (progn
;;    (require 'rtags)
;;    (cmake-ide-setup)))

;; modern-cpp-font-lock for modern c++ highlighting
(use-package modern-cpp-font-lock
  :ensure t)

;; electric-pair
(add-hook 'c-mode-hook 'electric-pair-mode)
(add-hook 'c++-mode-hook 'electric-pair-mode)
(add-hook 'python-mode-hook 'electric-pair-mode)

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

(use-package cmake-mode
  :ensure t)

;; Multi-term
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/usr/bin/bash"))

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

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package fancy-battery
  :ensure t
  :config
  (add-hook 'after-init-hook #'fancy-battery-mode)
  (setq fancy-battery-show-percentage t))

;; Misc
(set-face-attribute 'default nil :font "inconsolata-15") ; Menlo-15 is nice too
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
;; Open schedule in org-mode
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
  (org-agenda)
  )

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
(add-to-list 'default-frame-alist '(font . "inconsolata-15"))
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
 '(TeX-view-program-list
   (quote
    (("Emacs"
      ("emacsclient -e '(with-current-buffer (buffer-name) (find-file-other-window \"%o\"))'")
      "emacsclient"))))
 '(TeX-view-program-selection
   (quote
    (((output-pdf has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-pdf "Emacs")
     (output-dvi "PDF Tools")
     (output-html "xdg-open"))))
 '(custom-safe-themes
   (quote
    ("3cd4f09a44fe31e6dd65af9eb1f10dc00d5c2f1db31a427713a1784d7db7fdfc" default)))
 '(org-agenda-files (quote ("~/.schedule")))
 '(package-selected-packages
   (quote
    (multiple-cursors flycheck-pkg-config auctex fancy-battery yasnippet-snippets which-key use-package try treemacs-projectile treemacs-magit treemacs-icons-dired spaceline-all-the-icons rust-mode rainbow-delimiters org-bullets multi-term monokai-theme modern-cpp-font-lock gruvbox-theme flymd flycheck-rtags flycheck-irony exotica-theme exec-path-from-shell doom-themes diff-hl counsel-etags company-jedi company-irony company-c-headers cmake-mode cmake-ide clang-format))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
