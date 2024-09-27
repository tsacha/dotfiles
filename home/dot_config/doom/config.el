;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Sacha Trémoureux"
      user-mail-address "sacha@tremoureux.fr")

(defun tsacha/select-theme (&rest signal)
  "Select theme matching system settings"
  (cond
   ;;; Linux
   ((string-equal system-type "gnu/linux")
    (require 'gsettings)
    (if (string-equal (gsettings-get "org.gnome.desktop.interface" "gtk-theme") "Adwaita")
        (progn
          (load-theme 'doom-rose-pine-dawn t))
      (progn
        (load-theme 'doom-rose-pine t))))
   ;;; MacOS
   ((string-equal system-type "darwin")
    (if (string-equal ns-system-appearance "light")
        (progn
          (load-theme 'doom-rose-pine-dawn t))
      (progn
        (load-theme 'doom-rose-pine t))))))

;;; Register system settings hooks
(cond
 ((string-equal system-type "gnu/linux")
  (tsacha/select-theme)
  (require 'dbus)
  (dbus-register-signal :session nil "/ca/desrt/dconf/Writer/user" "ca.desrt.dconf.Writer" "Notify" 'tsacha/select-theme))
 ((string-equal system-type "darwin")
  (add-hook 'ns-system-appearance-change-functions #'tsacha/select-theme)))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


(after! evil-snipe
  (setq evil-snipe-scope 'buffer))

(if (string-equal system-type "gnu/linux")
    (setq doom-font (font-spec :family "Monaspace Argon" :size 20)
          doom-variable-pitch-font (font-spec :family "Monaspace Argon") ; inherits `doom-font''s :size
          doom-symbol-font (font-spec :family "Monaspace Argon" :size 20)
          doom-big-font (font-spec :family "Monaspace Argon" :size 23))
  (setq doom-font (font-spec :family "Monaspace Argon" :size 18)
        doom-variable-pitch-font (font-spec :family "Monaspace Argon") ; inherits `doom-font''s :size
        doom-symbol-font (font-spec :family "Apple Color Emoji" :size 21)
        doom-big-font (font-spec :family "Monaspace Argon" :size 18)))



(add-hook! fish-mode
  (add-hook 'before-save-hook #'fish_indent-before-save))

(add-hook! python-mode
  (blacken-mode))

(add-hook! terraform-mode
  (terraform-format-on-save-mode))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(after! projectile
  (setq projectile-project-search-path '(("~/Git" . 1) ("~/Git/Work" . 1)))
  (setq projectile-switch-project-action #'projectile-dired)

  (projectile-discover-projects-in-search-path))
(setq magit-repository-directories
      `(("~/Git/Work" . 1)
        ("~/Git" . 1)))
