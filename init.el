(setq package-selected-packages 
      '(evil
	evil-leader
	company
	dracula-theme
	lsp-mode
	company-lsp
	lsp-python
	use-package
	session
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

  ;; Both native (>= OSX 10.7) and "old style" fullscreen are supported. Customize `ns-use-native-fullscreen' to change style. For >= 10.7 native is the default.
  (setq ns-use-native-fullscreen nil)
  (global-set-key (kbd "C-x RET") `toggle-frame-fullscreen)
  (setup-meta-key-issue))


(defun setup-used-packages ()
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
    :init
    (require 'company-lsp)
    (push 'company-lsp company-backends)
    (add-hook 'after-init-hook 'global-company-mode))

  (use-package lsp-python
    :init
    (add-hook 'python-mode-hook #'lsp-python-enable))

  (use-package session
    :init
    (add-hook 'after-init-hook 'session-initialize))
  )


(defun setup-interface ()
  (desktop-save-mode 1)
  (scroll-bar-mode -1)
  (menu-bar-mode 0)
  (show-paren-mode t)
  (tool-bar-mode 0)
  (tooltip-mode 0))


(defun init ()
  ;; init scripts.
  (setup-package-manager)
  (setup-used-packages)
  (setup-keymapping)
  (setup-interface)
  )


(init)
