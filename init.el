;;; Package -- init.el
;;; Commentary:
;;; Code:

(setq evil-want-abbrev-expand-on-insert-exit nil)
(setq package-selected-packages
      '(evil
	evil-leader
	org-ref
	auctex
	bongo
	emms
	emms-bilibili
	sass-mode
	go
	go-mode
	company-coq
	search-web
	paredit
	ranger
	dired-ranger
	typescript-mode
	xwidgete
	multiple-cursors
	multi-term
	multi-eshell
        exwm
	sr-speedbar
	ecb
	zoom
	cedit
	ace-jump-mode
	el-get
	ack
	zone-matrix
	dumb-jump
	ctags
	projectile
	exec-path-from-shell
	nyan-mode
	zone-nyan
	company
	dracula-theme
	lsp-mode
	lsp-ui
	company-lsp
	lsp-python
	use-package
	session
	helm
	helm-pydoc
	helm-bibtexkey
	powerline
	spaceline
	eyebrowse
	persp-mode
	all-the-icons
	spaceline-all-the-icons
	linum
	linum-relative
	linum-off
	rust-mode
	lsp-rust
	rust-playground
	flycheck-rust
	flycheck-pos-tip
	flycheck-pyflakes
	flycheck-pycheckers
	imenu-list
	minimap
	elpy
	pyenv-mode
	markdown-mode+
	markdown-preview-mode
	latex-preview-pane
	pandoc
	pandoc-mode
	load-theme-buffer-local
	solarized-theme
	virtualenvwrapper
	virtualenv
	company-jedi
	writegood-mode
	writeroom-mode
	racer
	company-racer
	)
      )


(defun custom-packages ()
  "Setup comtom define packages."
  (el-get-bundle rate-sx
    :url "https://github.com/davep/rate-sx.el.git"
    )
  (el-get-bundle ProofGeneral
    :url "https://github.com/ProofGeneral/PG.git"
    :init
    (require 'proof-site "~/.emacs.d/el-get/ProofGeneral/generic/proof-site")
    )
  )


(defun setup-package-manager ()
  "Setup package manager."
  ;; support el-get
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
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
  "Ref: `https://www.emacswiki.org/emacs/MetaKeyProblems` \
mapping osx's command key to meta key."
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))


(defun setup-keymapping ()
  "Setup basic keymapping."
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
  (global-set-key (kbd "C-x C-b") `ibuffer)
  (setup-meta-key-issue))


(defun setup-common-packages ()
  "Ref: `https://github.com/CachesToCaches/getting_started_with_use_package/blob/master/init-use-package.el`."
  (eval-when-compile
    (require 'use-package))

  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize))

  (use-package flycheck
    :config
    (global-flycheck-mode)
    )

  (use-package flycheck-pycheckers
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
    )

  (use-package flyspell
    :config
    (flyspell-mode-on)
    )

  (use-package projectile)

  (use-package flycheck-pos-tip
    :init
    (flycheck-pos-tip-mode))

  (use-package dracula-theme
    :init
    (load-theme 'dracula t))

  (use-package evil
    :config
    :init
    (evil-mode 1))

  (use-package company
    :init
    (add-hook 'after-init-hook 'global-company-mode)
    :config
    (require 'company-lsp)
    (push 'company-lsp company-backends))

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
  (setq ring-bell-function 'ignore)
  (setq browse-url-browser-function 'xwidget-webkit-browse-url)
  (setq js-indent-level 2)
  (setq doc-view-continuous t)
;;  (zoom-mode t)
  (add-to-list 'default-frame-alist '(height . 40))
  (add-to-list 'default-frame-alist '(width . 160))
  (scroll-bar-mode -1)
  (menu-bar-mode 0)
  (show-paren-mode t)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (global-visual-line-mode 1)
  (global-prettify-symbols-mode)
  (global-visual-line-mode 1)

  (use-package spaceline
    :config
    (setq ns-use-srgb-colorspace nil)
    (setq spaceline-responsive nil)
    )

  (use-package undo-tree
    :init
    (global-undo-tree-mode)
    )

  (use-package ace-jump-mode
    :init
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
    )

  (use-package dumb-jump
    :init
    (dumb-jump-mode))

  (use-package neotree
    :config
    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
    (global-set-key (kbd "M-s") 'neotree-toggle)
    (setq neo-smart-open t)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    )

  (use-package spaceline-all-the-icons
    :after
    spaceline
    :config
    (setq powerline-scale 1)
    :init
    (set-face-attribute 'mode-line nil  :height 120)
    (spaceline-all-the-icons-theme))

  (use-package ecb)

  (use-package ido
    :config
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (setq ido-use-filename-at-point 'guess)
    (ido-mode 1)
    (global-set-key (kbd "C-x C-f") 'ido-find-file))

  (use-package helm
    :after spaceline
    :init
    (helm-mode 1)
    (spaceline-helm-mode)
    :config
    (global-set-key (kbd "M-i") 'helm-imenu)
    (global-set-key (kbd "M-x") 'helm-M-x))

  (use-package minimap
    :config
     (global-set-key (kbd "M-m") 'minimap-mode)
    )
  (use-package linum-off
    :config
    (setq linum-disabled-modes-list
	  '(eshell-mode)
	  ))


  (use-package linum-relative
    :after
    helm
    :config
    (set-face-foreground 'linum "SkyBlue2")
    (set-face-attribute 'linum nil :height 120)
    (setq linum-relative-current-symbol "")
    (setq linum-relative-format "%3s ")
    :init
    (add-hook 'text-mode-hook 'linum-relative-mode)
    (add-hook 'prog-mode-hook 'linum-relative-mode)
    (add-hook 'typescript-mode-hook 'linum-relative-mode)

    (global-hl-line-mode t))


  (use-package nyan-mode
    :config
    (setq nyan-wavy-trail t)
    :init
    (nyan-mode))

  (use-package load-theme-buffer-local
    :config
    (add-hook 'neotree-mode-hook (lambda nil (load-theme-buffer-local 'wombat (current-buffer))))
    )

  (use-package xwidget
    :config
    (define-key xwidget-webkit-mode-map [mouse-6] 'xwidget-webkit-scroll-down)
    (define-key xwidget-webkit-mode-map [mouse-7] 'xwidget-webkit-scroll-up)
    (define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-up)
    (define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-down)
    )

  (use-package xwidgete)

  (use-package search-web
    :config
    (require 'search-web)
    (global-set-key (kbd "C-c w") 'search-web)
    (defun browse-url-default-browser (url &rest args)
      "Override `browse-url-default-browser' to use `xwidget-webkit' URL ARGS."
      (xwidget-webkit-browse-url url args))
    )
  (use-package emms
    :init
    (require 'emms-setup)
    (emms-standard)
    (emms-default-players)
    )
  )

(defun setup-langs ()
  "Setup langauge env."
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (require 'proof-site "~/.emacs.d/el-get/ProofGeneral/generic/proof-site")

  (use-package flycheck-rust)

  (use-package lisp-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
    )

  (use-package rust-mode
    :config
    (add-hook 'rust-mode-hook 'racer-mode))

  (use-package elpy
    :init
    (elpy-enable)
    :config
    (add-hook 'python-mode-hook 'elpy-mode)
    )

  (use-package lsp-python
    :config
;;    (add-hook 'python-mode-hook 'lsp-python-enable)
    )

  (use-package lsp-ui
    :config
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    )

  (use-package virtualenvwrapper
    :config
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    (setq venv-location "/Users/ryan/Envs")
    )

  (use-package virtualenv
    :config
    )

  (use-package company-jedi
    :config
    (add-to-list 'company-backends 'company-jedi)
    )

  (use-package typescript-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
    )

  (use-package company-coq
    :config
    (add-hook 'coq-mode-hook 'company-coq-mode)
    )
  (use-package markdown-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.mkd\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    )

    (use-package latex-preview-pane
      :init
      (add-hook 'latex-mode-hook 'latex-preview-pane)
      )

    (use-package org
      :config
      (setq org-link-abbrev-alist
	    '(("bib" . "~/research/refs.bib::%s")
	      ("notes" . "~/research/notes/notes.org::#%s")
	      ("papers" . "~/research/papers/%s.pdf")))
      (defun org-mode-reftex-search ()
	;;jump to the notes for the paper pointed to at from reftex search
	(interactive)
	(org-open-link-from-string (format "[[notes:%s]]" (first (reftex-citation t)))))
      (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
      (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search)
      )

    (use-package reftex
      :config
      ;;  https://www.anand-iyer.com/blog/2017/research-literature-management-with-emacs.html
      ;;  https://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/
      (and (buffer-file-name) (file-exists-p (buffer-file-name))
	   (progn
	     (global-auto-revert-mode t)
	     (reftex-parse-all)
	     (reftex-set-cite-format
	      '((?b . "[[bib:%l][%l-bib]]")
		(?n . "[[notes:%l][%l-notes]]")
		(?p . "[[papers:%l][%l-paper]]")
		(?t . "%t")
		(?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
      (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
      (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
      )

    (use-package markdown-preview-mode
      :config
      (setq browse-url-browser-function 'xwidget-webkit-browse-url)
      )
    (use-package go-mode)
    (use-package writegood-mode
      :config
      (add-hook 'markdown-mode-hook writegood-mode)
      (add-hook 'org-mode-hook writegood-mode)
      (add-hook 'latex-mode-hook writegood-mode)
      (add-hook 'writeroom-mode writegood-mode)
      )
    )

(defun init ()
  "Init scripts."
  (setup-package-manager)
  (setup-common-packages)
  (setup-keymapping)
  (setup-interface)
  (setup-langs))


(init)
(provide 'init)
;;; init.el ends here
