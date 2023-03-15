
(setq inhibit-startup-message t)

(scroll-bar-mode -1)    ;; Disable visible scrollbar
(tool-bar-mode -1)      ;; Disable the toolbar
(tooltip-mode -1)       ;; Disable tooltips
(set-fringe-mode 10)    ;; Give some breathing room

(menu-bar-mode -1)      ;; Disable the menu bar

(setq visible-bell nil) ;; Set up the visible bell

(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120)
(set-face-attribute 'bold nil :font "DejaVu Sans Mono bold" :height 120)

(require 'package) ;; Initialize package sources

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; (column-number-mode)
;; (global-display-line-numbers-mode t)

;; ;; Disable line numbers for some modes
;; (dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode)

(use-package swiper)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom (ivy-mode 1))

;; Run on first install: M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  ;; :init (load-theme 'doom-palenight t))
  :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(defun erc-next-buffer (&optional reverse)
  (interactive)
  (let* ((buffers (mapcar
		   (lambda (x) (car x))
		   (sort (mapcar
			  (lambda (x) (cons x (downcase (replace-regexp-in-string "^#*" "" x))))
			  (copy-list (erc-all-buffer-names)))
			 (lambda (a b)
			   (string< (cdr a) (cdr b))))))
	 (active (buffer-name (current-buffer))))
    (let* ((reordered (if reverse (reverse buffers) buffers))
	   (m (member active reordered))
	   (next (or (cadr m) (car reordered))))
      (switch-to-buffer next))))

(defun erc-prev-buffer ()
  (interactive)
  (erc-next-buffer t))

(add-hook 'erc-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-]") 'erc-next-buffer)
	    (local-set-key (kbd "M-[") 'erc-prev-buffer)))

(use-package erc
  :init
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  :config
  (erc-lurker-initialize))

(use-package znc
  :after erc)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package lua-mode)

(use-package vterm
  :ensure t
  :init
  (defvar vterm-install t))

(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region)
  ("C--" . 'er/contract-region))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :init
  (setq org-directory (concat user-emacs-directory "org")))

(use-package magit)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load custom-file))
