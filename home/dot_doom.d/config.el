;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;;; Latitude/Longitude
(setq calendar-latitude +47.23)
(setq calendar-longitude -1.63)
(setq calendar-location-name "Nantes, France")

;;; Startup messages
(setq inhibit-startup-message t
      initial-scratch-message ""
      inhibit-startup-echo-area-message t)

;;;; Personal informations
(setq user-full-name "Sacha Trémoureux"
      user-mail-address "sacha@tremoureux.fr")

;;;; History
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;;;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;;; Sentences end with a single space
(setq sentence-end-double-space nil)


;;;;; Mouse autoselect
(setq mouse-autoselect-window t)

;;;; Buffer / File Warnings
(setq confirm-nonexistent-file-or-buffer nil)
;; via https://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun tsacha/create-non-existent-directory ()
  "Check whether a given file's parent directories exist; if they do not, offer to create them."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'tsacha/create-non-existent-directory)

;;;; yes/no questions
(setq use-short-answers t)

;;;; Clock
(display-time-mode 1)
(setq display-time-default-load-average nil)

;;;; GPG agent
(setenv "SSH_AUTH_SOCK" "/run/user/1000/gnupg/S.gpg-agent.ssh")

;;;; GPG
(setq epg-gpg-program "gpg2")

;;;; Themes
(setq custom-safe-themes t)

(defun tsacha/select-theme (&rest signal)
  "Select theme matching system settings"
  (cond
   ;;; Linux
   ((string-equal system-type "gnu/linux")
    (require 'gsettings)
    (if (string-equal (gsettings-get "org.gnome.desktop.interface" "gtk-theme") "Adwaita")
        (progn
          (load-theme 'doom-gruvbox-light t))
      (progn
        (load-theme 'doom-gruvbox t))))
   ;;; MacOS
   ((string-equal system-type "darwin")
    (if (string-equal ns-system-appearance "light")
        (progn
          (load-theme 'doom-gruvbox-light t))
      (progn
        (load-theme 'doom-gruvbox t))))))

;;; Register system settings hooks
(cond
 ((string-equal system-type "gnu/linux")
  (tsacha/select-theme)
  (require 'dbus)
  (dbus-register-signal :session nil "/ca/desrt/dconf/Writer/user" "ca.desrt.dconf.Writer" "Notify" 'tsacha/select-theme))
 ((string-equal system-type "darwin")
  (add-hook 'ns-system-appearance-change-functions #'tsacha/select-theme)))

;;;; Fonts
(if (string-equal system-type "gnu/linux")
    (setq doom-font (font-spec :family "Iosevka" :size 18 :weight 'semi-light)
          doom-variable-pitch-font (font-spec :family "Iosevka") ; inherits `doom-font''s :size
          doom-unicode-font (font-spec :family "Iosevka" :size 18)
          doom-big-font (font-spec :family "Iosevka" :size 21))
  (setq doom-font (font-spec :family "Iosevka" :size 16 :weight 'semi-light)
        doom-variable-pitch-font (font-spec :family "Iosevka") ; inherits `doom-font''s :size
        doom-unicode-font (font-spec :family "Apple Color Emoji" :size 16)
        doom-big-font (font-spec :family "Iosevka" :size 18)))

;;; Keybinds
(map! "<f1>" #'other-window)
(map! "<f2>" #'consult-buffer)
(map! "<f3>" #'projectile-switch-project)
(map! "<f4>" #'projectile-find-file)
(map! "<f5>" #'vterm)
(map! "C-x p" #'projectile-switch-project)
(map! "C-x z" #'zoom-window-zoom)
(map! "M-+" #'undo-fu-only-undo)
(map! "M--" #'undo-fu-only-redo)
(map! "C-s" #'consult-line)
(map! "C-M-*" #'tsacha/directory-search)
(map! "C-M-=" #'tsacha/project-search)
(map! "C-x f" #'consult-find)
(map! "C-x g" #'magit)
(map! "C-S-c" #'clipboard-kill-ring-save)
(map! "C-S-x" #'clipboard-kill-region)
(map! "C-S-v" #'clipboard-yank)
(map! "C-<f1>" #'magit-list-repositories)

(defun tsacha/directory-search (&optional arg initial-query directory)
  (interactive "P")
  (consult-ripgrep default-directory))

(defun tsacha/project-search (&optional arg initial-query directory)
  (interactive "P")
  (consult-git-grep))

(after! projectile
  (setq projectile-project-search-path '(("~/Git" . 1) ("~/Git/Work" . 1)))
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-discover-projects-in-search-path))

(setq zoom-window-mode-line-color "DarkGreen")

(after! ivy
  (setq ivy-extra-directories '("../" "./")))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook! fish-mode
  (add-hook 'before-save-hook #'fish_indent-before-save))

(add-hook! python-mode
  (blacken-mode))

(add-hook! terraform-mode
  (terraform-format-on-save-mode))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;; Completion
(after! vertico
  (vertico-prescient-mode)
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((find-file (vertico-sort-function . vertico-sort-alpha))
          (projectile-find-file (vertico-sort-function . vertico-sort-history-alpha)))))

;; Org
(after! org
  (setq org-todo-keywords (quote ((sequence "TODO(t)" "IN PROGRESS(p)" "|" "DONE(d)")
                                  (sequence "WAITING(w)" "|" "CANCELLED(c)")))
        org-directory "~/Git/Notes"
        org-agenda-files (list
                          "~/Git/Notes/Perso/gtd.org"
                          "~/Git/Notes/Work/gtd.org")
        org-fold-core-style 'overlays
        org-todo-keyword-faces (quote
                                (("TODO" :foreground "red" :weight bold)
                                 ("IN PROGRESS" :foreground "blue" :weight bold)
                                 ("DONE" :foreground "forest green" :weight bold)
                                 ("WAITING" :foreground "orange" :weight bold)
                                 ("CANCELLED" :foreground "forest green" :weight bold)))))

(setq magit-repository-directories
      `(("~/Git/Work" . 1)
        ("~/Git" . 1)))
(setq magit-repolist-columns
      '(("Name" 25 magit-repolist-column-ident nil)
        ("Status" 7 magit-repolist-column-flag nil)
        ("B<U" 3 magit-repolist-column-unpulled-from-upstream
         ((:right-align t)
          (:help-echo "Upstream changes not in branch")))
        ("B>U" 3 magit-repolist-column-unpushed-to-upstream
         ((:right-align t)
          (:help-echo "Local changes not in upstream")))
        ("Path" 99 magit-repolist-column-path nil)))
