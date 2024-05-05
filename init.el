
;; (column-number-mode)
;; (global-display-line-numbers-mode t)

;; (dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))

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


;;; from evil

(defmacro save-side-windows (&rest body)
  (declare (indent defun) (debug (&rest form)))
  (let ((sides (make-symbol "sidesvar")))
    `(let ((,sides (and (functionp 'window-toggle-side-windows)
                        (window-with-parameter 'window-side))))
       (when ,sides
         (window-toggle-side-windows))
       (unwind-protect
           (progn ,@body)
         (when ,sides
           (window-toggle-side-windows))))))

(defun move-window (side)
  (save-side-windows
   (unless (one-window-p)
     (save-excursion
       (let ((w (window-state-get (selected-window))))
	 (delete-window)
	 (let ((wtree (window-state-get)))
	   (delete-other-windows)
	   (let ((subwin (selected-window))
		 ;; NOTE: SIDE is new in Emacs 24
		 (newwin (split-window nil nil side)))
	     (window-state-put wtree subwin)
	     (window-state-put w newwin)
	     (select-window newwin)))))
     (balance-windows))))

(defun move-window-above ()
  (interactive)
  (move-window 'above))

(defun move-window-below ()
  (interactive)
  (move-window 'below))

(defun move-window-left ()
  (interactive)
  (move-window 'left))

(defun move-window-right ()
  (interactive)
  (move-window 'right))


(require 'package)

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

(use-package erc
  :init
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  :config
  (erc-lurker-initialize)
  (add-hook 'erc-mode-hook
	    (lambda ()
	      (local-set-key (kbd "M-]") 'erc-next-buffer)
	      (local-set-key (kbd "M-[") 'erc-prev-buffer))))

(use-package znc
  :after erc)

;; (use-package slime
;;   :config
;;   (setq inferior-lisp-program "sbcl"))

(use-package sly)

(use-package lua-mode)

(use-package vterm
  :ensure t
  :init
  (defvar vterm-install t))

(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region)
  ("C--" . 'er/contract-region))

;; (use-package org
;;   :init
;;   (setq org-directory (concat user-emacs-directory "org")))

(use-package magit)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package company
  :ensure t
  :init
  (setq company-idle-delay nil)
  :bind
  ("C-i" . 'company-indent-or-complete-common))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load custom-file))
