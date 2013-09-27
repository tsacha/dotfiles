;; C
(add-hook 'c-mode-common-hook
          '(lambda()
             (local-set-key (kbd "RET") 'newline-and-indent)
             (c-set-style "awk")
             ))
;; Python
(add-hook 'python-mode-hook
          '(lambda()
             (local-set-key (kbd "RET") 'newline-and-indent)
             ))

(add-hook 'inferior-python-mode-hook
          '(lambda()
             (python-send-string "sys.path.append('')")
             ))

;; Perl

(add-hook 'perl-mode-hook
          '(lambda()
             (local-set-key (kbd "RET") 'newline-and-indent)
             ))

;; Android
(add-hook 'java-mode-hook
          '(lambda()
             (local-set-key (kbd "C-c C-t C-a") 'android-compile-command)
             ))

;; Lisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             (local-set-key (kbd "RET") 'newline-and-indent)
             ))


;; LaTeX (AucTeX)

(require 'tex-site)
(add-hook 'TeX-mode-hook 'TeX-PDF-mode)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(setq TeX-view-program-list '(("Okular" "okular %o")))
(setq TeX-view-program-selection '((output-pdf "Okular")))

;;;;;;;; Doc ;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun markdown-custom ()
    "markdown-mode-hook"
      (setq markdown-command "pandoc -s -B /home/sacha/Documents/Technology/Miscellaneous/markdown.css"))


(add-hook 'markdown-mode-hook
	  '(lambda()
	     (markdown-custom)
	     (local-set-key (kbd "M-n") 'yank)
	     ))


;; PHP
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; CSS
(add-hook 'css-mode-hook
          '(lambda()
             (local-set-key (kbd "RET") 'newline-and-indent)
             ))

;; JavaScript
(add-hook 'js-mode-hook
          '(lambda()
             (local-set-key (kbd "RET") 'newline-and-indent)
             ))
