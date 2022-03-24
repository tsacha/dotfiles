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


;;;; Buffer / File Warnings
(setq confirm-nonexistent-file-or-buffer nil)
;; via https://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun create-non-existent-directory ()
  "Check whether a given file's parent directories exist; if they do not, offer to create them."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'create-non-existent-directory)

;;;; yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Clock
(display-time-mode 1)
(setq display-time-default-load-average nil)

;;;; GPG agent
(setenv "SSH_AUTH_SOCK" "/run/user/1000/gnupg/S.gpg-agent.ssh")

;;;; GPG
(setq epg-gpg-program "gpg2")

;;;; Themes
(setq custom-safe-themes t)
(cond
 ((string-equal system-type "gnu/linux")
  (use-package! gsettings
    :config
    (defun tsacha/reload-theme (&rest signal)
      "Reload theme"
      (if (string-equal (gsettings-get "org.gnome.desktop.interface" "gtk-theme") "Adwaita")
          (progn
            (load-theme 'doom-gruvbox-light t))
        (progn
          (load-theme 'doom-gruvbox t))))
    (tsacha/reload-theme)
    (require 'dbus)
    (dbus-register-signal :session nil "/ca/desrt/dconf/Writer/user" "ca.desrt.dconf.Writer" "Notify" 'tsacha/reload-theme)))
 ((string-equal system-type "darwin")

  (defun my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'doom-gruvbox-light t))
      ('dark (load-theme 'doom-gruvbox t))))
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)))

;;;; Fonts
(if (string-equal system-type "gnu/linux")
    (setq doom-font (font-spec :family "Iosevka" :size 18 :weight 'semi-light)
          doom-variable-pitch-font (font-spec :family "Iosevka") ; inherits `doom-font''s :size
          doom-unicode-font (font-spec :family "Iosevka" :size 18)
          doom-big-font (font-spec :family "Iosevka" :size 21))
  (setq doom-font (font-spec :family "Iosevka" :size 16 :weight 'semi-light)
        doom-variable-pitch-font (font-spec :family "Iosevka") ; inherits `doom-font''s :size
        doom-unicode-font (font-spec :family "Iosevka" :size 16)
        doom-big-font (font-spec :family "Iosevka" :size 18)))

;;; Keybinds
(map! "<f2>" #'treemacs)
(map! "M-$" #'other-window)
(map! "M-+" #'undo-fu-only-undo)
(map! "M--" #'undo-fu-only-redo)
(map! "C-s" #'swiper)
(map! "C-M-/" #'counsel-git)
(map! "C-M-*" #'counsel-git-grep)
(map! "C-M-=" #'counsel-ag)
(map! "C-x g" #'magit)
(map! "C-S-c" #'clipboard-kill-ring-save)
(map! "C-S-x" #'clipboard-kill-region)
(map! "C-S-v" #'clipboard-yank)
(map! "C-x b" #'list-buffers)

(after! ivy
  (setq ivy-extra-directories '("../" "./")))



(add-hook! python-mode
  (blacken-mode))

(add-hook! terraform-mode
  (terraform-format-on-save-mode))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
