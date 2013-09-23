(set-locale-environment "utf-8")
(setenv "LANG"  "fr_FR.UTF-8")

(defun sacha/init-emacs ()
  "Load various emacs init files"
  (interactive)
  (dolist (file (directory-files "~/.config/emacs" nil "^init-[0-9]+-.+\.el$"))
    (load (concat "~/.config/emacs/" file))))
(sacha/init-emacs)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((erlang-indent-level . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

