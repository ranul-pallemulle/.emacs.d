;;; package --- Summary:
;;; Commentary:
;;; Code:
(unless (version< emacs-version "26.0")

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
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook 'turn-on-auto-fill)

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
    :config
    (global-flycheck-mode t)
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-checker 'c/c++-gcc)))
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-c/c++-gcc-executable "/usr/local/Cellar/gcc/9.1.0/bin/g++-9")))
    (add-hook 'c-mode-hook (lambda () (setq flycheck-checker 'c/c++-gcc)))
    (add-hook 'c-mode-hook (lambda () (setq flycheck-gcc-language-standard "gnu99")))
    )

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

  ;; Rust
  (use-package rust-mode
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

  ;; treemacs
  (use-package treemacs
    :ensure t
    :defer t
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "s-0") #'treemacs))
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
            treemacs-width                         35)

      ;; The default width and height of the icons is 22 pixels. If you are
      ;; using a Hi-DPI display, uncomment this to double the icon size.
      (treemacs-resize-icons 44)

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
          ("s-0"       . treemacs) ; default: treemacs-select-window
          ("C-x t 1"   . treemacs-delete-other-windows)
          ("C-x t t"   . treemacs)
          ("C-x t B"   . treemacs-bookmark)
          ("C-x t C-t" . treemacs-find-file)
          ("C-x t M-t" . treemacs-find-tag)))

  (use-package treemacs-icons-dired
    :after treemacs dired
    :ensure t
    :config (treemacs-icons-dired-mode))

  (use-package treemacs-magit
    :after treemacs magit
    :ensure t)

  ;; Themes
  (use-package all-the-icons		;required by doom-neotree, spaceline
    :ensure t)

  (use-package doom-themes
    :ensure t
    :config
    (load-theme 'doom-spacegrey t)
    (setq doom-themes-enable-bold t
	  doom-themes-enable-italic t)
    (doom-themes-treemacs-config)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))
  
  ;; spaceline
  (use-package spaceline
    :ensure t
    :config
    (spaceline-emacs-theme)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified))
  (use-package spaceline-all-the-icons
	       :after spaceline
	       :config
	       (spaceline-all-the-icons-theme))

  (setq spaceline-all-the-icons-separator-type 'wave
   spaceline-separator-dir-left '(left . left)
   spaceline-separator-dir-right '(right . right)
   )

  (setq spaceline-all-the-icons-icon-set-flycheck-slim 'dots
        spaceline-all-the-icons-icon-set-git-ahead 'commit
        spaceline-all-the-icons-flycheck-alternate t
        spaceline-all-the-icons-highlight-file-name t
        spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (setq spaceline-all-the-icons-hide-long-buffer-path t)

  (set-face-attribute 'spaceline-unmodified nil :background "#ed9442") ; LightSkyBlue
  (set-face-attribute 'spaceline-modified nil :background "#ef6034") ; #f7e165

  ;; Git porcelain
  (use-package magit
    :ensure t
    :bind (("C-x g" . magit-status)))

  ;; diff-hl - for highlighting uncommitted changes
  (use-package diff-hl
    :ensure t
    :config
    (global-diff-hl-mode t)
    (diff-hl-flydiff-mode t)
    (diff-hl-margin-mode t)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  ;; Make emacs agree with shell on mac
  (use-package exec-path-from-shell
    :ensure t
    :config
    ;; (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "CPLUS_INCLUDE_PATH")
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
  (add-hook 'c-mode-hook 'electric-pair-mode)
  (add-hook 'c++-mode-hook 'electric-pair-mode)
  (add-hook 'python-mode-hook 'electric-pair-mode)
  (add-hook 'rust-mode-hook 'electric-pair-mode)

  ;; 4 space indent in ccmodes
  (setq c-basic-offset 4)

  ;;quick compile
  (with-eval-after-load 'cc-mode
    (define-key c-mode-base-map (kbd "M-c") 'compile))

  ;; windmove
  ;;(windmove-default-keybindings) ;; not using these anymore
  (global-set-key (kbd "s-w") 'windmove-up)
  (global-set-key (kbd "s-s") 'windmove-down)
  (global-set-key (kbd "s-a") 'windmove-left)
  (global-set-key (kbd "s-d") 'windmove-right)

  ;; Python
  (setq python-shell-interpreter "python3")

  ;; Misc
  (global-hl-line-mode t)
  (set-face-attribute 'default nil :font "Monaco-15") ; Menlo-15 is nice too
  ;;(setq mac-command-modifier 'meta) ; make command function as alt key
  (when (memq window-system '(mac ns x))
  (scroll-bar-mode -1))
  (global-display-line-numbers-mode)
  (add-hook 'org-mode-hook (lambda() (display-line-numbers-mode -1)))
  (column-number-mode t)
  (add-hook 'c-mode-common-hook 'auto-fill-mode)
  (menu-bar-mode -1)
  (global-set-key (kbd "M-<backspace>") 'delete-forward-char)
  (add-hook
   'eww-mode-hook
   (lambda()(define-key eww-mode-map (kbd "M-c") 'eww-toggle-colors)))

  (use-package fancy-battery
    :ensure t
    :config
    (add-hook 'after-init-hook #'fancy-battery-mode))

  ;; open init.el by "M-x init"
  (defun init ()
    "Edit the `user-init-file', in another window."
    (interactive)
    ;; (find-file-other-window user-init-file))
    (find-file user-init-file)
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
	  )
      (progn
					;(message "Showing shell")
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
;  (setq remote-file-name-inhibit-cache nil)
;  (setq vc-ignore-dir-regexp
;	(format "%s\\|%s"
;                vc-ignore-dir-regexp
;                tramp-file-name-regexp))
  (setq tramp-verbose 1)

  ;; some environment variables
  (let ((unidir (shell-command-to-string ". ~/.bash_profile; echo -n $unidir")))
    (setenv "unidir" unidir))
  (let ((mars (shell-command-to-string ". ~/.bash_profile; echo -n $mars")))
    (setenv "mars" mars))
  (let ((ares (shell-command-to-string ". ~/.bash_profile; echo -n $ares")))
    (setenv "ares" ares))
  (let ((nektar (shell-command-to-string ". ~/.bash_profile; echo -n $nektar")))
    (setenv "nektar" nektar))
  (let ((fyp (shell-command-to-string ". ~/.bash_profile; echo -n $fyp")))
    (setenv "fyp" fyp))
  )

(setq default-directory "~/")

(provide 'init)
;;; init.el ends here
