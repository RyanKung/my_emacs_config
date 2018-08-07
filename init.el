(setq package-selected-packages 
      '(evil
	evil-leader
	exec-path-from-shell
	nyan-mode
	zone-nyan
	company
	dracula-theme
	lsp-mode
	company-lsp
	lsp-python
	use-package
	session
	helm
	powerline
	spaceline
	eyebrowse
	persp-mode
	all-the-icons
	spaceline-all-the-icons
	nlinum
	nlinum-relative
	;; rust
	rust-mode
	lsp-rust
	flymake-rust
	)
      )


(defun setup-package-manager ()
  ;; setup package manager
  (setq package-archives
	'(("gnu" . "https://elpa.gnu.org/packages/")
          ("marmalade" . "https://marmalade-repo.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  ;; install `required package
  ;; ref https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))


(defun setup-meta-key-issue ()
  ;; ref: `https://www.emacswiki.org/emacs/MetaKeyProblems`
  ;; mapping osx's command key to meta key.
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))


(defun setup-keymapping ()
  ;; setup basic keymapping.
  (use-package evil-leader
    :init
    (evil-leader/set-key
      "e" 'find-file
      "b" 'switch-to-buffer
      "k" 'kill-buffer
      "d" 'dired
      "z" 'repeat
      "0" 'delete-window
      "1" 'delete-other-windows
      "2" 'split-window-below
      "3" 'split-window-right)
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode))
  ;; https://www.emacswiki.org/emacs/YesOrNoP
  (defalias 'yes-or-no-p 'y-or-n-p) 
  ;; Both native (>= OSX 10.7) and "old style" fullscreen are supported. Customize `ns-use-native-fullscreen' to change style. For >= 10.7 native is the default.
  (setq ns-use-native-fullscreen nil)
  (global-set-key (kbd "C-x RET") `toggle-frame-fullscreen)
  (global-set-key (kbd "C-<tab>") `other-window)
  (setup-meta-key-issue))


(defun setup-common-packages ()
  ;; ref: https://github.com/CachesToCaches/getting_started_with_use_package/blob/master/init-use-package.el
  (eval-when-compile
    (require 'use-package))

  (use-package dracula-theme
    :init
    (load-theme 'dracula t))
  (use-package evil
    :init
    (evil-mode 1))

  (use-package company
    :config
    (require 'company-lsp)
    (push 'company-lsp company-backends)
    (add-hook 'after-init-hook 'global-company-mode))

  (use-package lsp-python
    :config
    (add-hook 'python-mode-hook #'lsp-python-enable))

  (use-package eyebrowse
    :init
    (eyebrowse-mode t)
    )
  (use-package persp-mode
    :init
    (persp-mode t)
    )
  (use-package session
    :config
    (add-hook 'after-init-hook 'session-initialize))
  )


(defun setup-interface ()
  ;;(desktop-save-mode 1)
  (setq ring-bell-function 'ignore)
  (scroll-bar-mode -1)
  (menu-bar-mode 0)
  (show-paren-mode t)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (global-visual-line-mode 1)
  (use-package spaceline
    :config
    (setq ns-use-srgb-colorspace nil)
    (setq spaceline-responsive nil)
     )

  (use-package neotree
    :config
    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
    (setq neo-theme 'icons)
    )

  (use-package spaceline-all-the-icons 
    :after
    spaceline
    :config
    (setq powerline-scale 1)
    :init
    (set-face-attribute 'mode-line nil  :height 120)
    (spaceline-all-the-icons-theme))

  (use-package helm
    :after spaceline
    :init
    (helm-mode 1)
    (spaceline-helm-mode)
    :config
    (global-set-key (kbd "M-x") 'helm-M-x))

  (use-package nlinum-relative
    :after
    helm
    :config
    (set-face-foreground 'linum "SkyBlue2")
    (set-face-attribute 'linum nil :height 120)
    (setq linum-relative-current-symbol "")
    (setq nlinum-relative-redisplay-delay 0)
    (setq nlinum-format "%d ")
    :init
    (global-hl-line-mode t)
    (nlinum-relative-setup-evil) 
    ;;    (global-nlinum-mode t)
    (nlinum-relative-mode t))


  (use-package nyan-mode
    :config
    (setq nyan-wavy-trail t)
    :init
    (nyan-mode))
  )

(defun setup-langs ()
  (use-package flymake-rust)
  (use-package rust-mode
    :hook
    (flymake-rust-load)
    :config
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))
  )
    

(defun init ()
  ;; init scripts.
;; fixed osx path issue
  (exec-path-from-shell-initialize)
  (setup-package-manager)
  (setup-common-packages)
  (setup-keymapping)
  (setup-interface)
  (setup-langs)
 )

(init)
