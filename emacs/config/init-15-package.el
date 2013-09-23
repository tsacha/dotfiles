(require 'package)

;; Repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; Packages
(setq sacha/packages '(
			  undo-tree
			  css-mode
			  auctex
			  php-mode
			  markdown-mode
			  org
			  linum
			  )
      )
(let ((refreshed nil))
  (dolist (package sacha/packages)
    (unless (package-installed-p package)
      (message (concat "Installing missing package: " (symbol-name package)))
      (unless refreshed
	(package-refresh-contents)
	(setq refreshed t))
      (package-install package))))
