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
(setq org-disputed-keys '(([(shift up)] . [(meta p)])
                         ([(shift down)] . [(meta n)])
                         ([(shift left)] . [(meta -)])
                         ([(shift right)] . [(meta +)])
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
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11"))))

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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         28)


    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(with-eval-after-load 'treemacs
(set-face-attribute 'treemacs-directory-face nil :font "inconsolata-16")
(set-face-attribute 'treemacs-file-face nil :font "inconsolata-12")
(set-face-attribute 'treemacs-root-face nil :font "inconsolata-16"))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package all-the-icons
  :ensure t)

;; Themes
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-spacegrey t)		;doom-vibrant, doom-spacegrey
  (doom-themes-org-config)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

; spaceline
(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified))

(use-package spaceline-all-the-icons
 :after spaceline
 :config
 (spaceline-all-the-icons-theme))

(setq spaceline-all-the-icons-separator-type 'wave) ;slant is default
(setq spaceline-all-the-icons-file-name-highlight "#42f4c8")
(setq spaceline-all-the-icons-hide-long-buffer-path t)

(set-face-attribute 'spaceline-unmodified nil :background "#ed9442") ; LightSkyBlue
(set-face-attribute 'spaceline-modified nil :background "#ef6034") ; #f7e165

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
  ;; (when (memq window-system '(mac ns x))
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
    (define-key c-mode-base-map (kbd "M-.") (function rtags-find-symbol-at-point))
    (define-key c-mode-base-map (kbd "M-,") (function rtags-find-references-at-point))
    (define-key c-mode-base-map (kbd "M-(") (function rtags-location-stack-back))
    (define-key c-mode-base-map (kbd "M-)") (function rtags-find-symbol-at-point))
    (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))))

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

;; Misc
(set-face-attribute 'default nil :font "inconsolata-15") ; Menlo-15 is nice too
(scroll-bar-mode -1)
(global-display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda() (display-line-numbers-mode -1)))
(add-hook 'doc-view-mode-hook (lambda() (display-line-numbers-mode -1)))
(column-number-mode t)
(add-hook 'c-mode-common-hook 'auto-fill-mode)
(menu-bar-mode -1)
(add-hook 'eww-mode-hook
	  (lambda()(define-key eww-mode-map (kbd "M-c") 'eww-toggle-colors)))
(global-set-key (kbd "C-t") (lookup-key global-map (kbd "C-x 5")))
(global-set-key (kbd "s-u") 'revert-buffer)
(use-package fancy-battery
  :ensure t
  :config
  (add-hook 'after-init-hook #'fancy-battery-mode))

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
(define-key c-mode-base-map (kbd "M-c") 'compile)

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

;; ;; mouse autofocus window on hover
;; (setq mouse-autoselect-window t)

(global-set-key (kbd "C-9") (lambda()
			      (interactive)
			      (insert-char ?\\)))
(global-set-key (kbd "C-0") (lambda()
			      (interactive)
			      (insert-char ?|)))

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
 '(package-selected-packages
   (quote
    (auctex fancy-battery yasnippet-snippets which-key use-package try treemacs-projectile treemacs-magit treemacs-icons-dired spaceline-all-the-icons rust-mode rainbow-delimiters org-bullets multi-term monokai-theme modern-cpp-font-lock gruvbox-theme flymd flycheck-rtags flycheck-irony exotica-theme exec-path-from-shell doom-themes diff-hl counsel-etags company-jedi company-irony company-c-headers cmake-mode cmake-ide clang-format))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
