
(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120)
(set-face-attribute 'bold nil :font "DejaVu Sans Mono bold" :height 120)

(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq custom-file (concat user-emacs-directory "custom.el"))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

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
