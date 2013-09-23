;; UTF-8

(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; Customisation de base

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Texte normal

(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq-default fill-column 80)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; No menu bar, tool bar, scroll bar

;; (menu-bar-mode 0)
;;(tool-bar-mode 0)
;;(scroll-bar-mode 0)

;; No beep, but flash screen
(setq visible-bell t)

;; Move point to top/bottom of buffer before signalling a scrolling error
(setq scroll-error-top-bottom t)


;; Display file name in the window title bar
(setq frame-title-format '(buffer-file-name "%b [%f]" "%b"))

;; Avoid sentences that end with 2 spaces (American style).
;; TODO: change this automatically according to the current dictionary
(setq sentence-end-double-space nil)

;; Avoid breaking lines at '(' or ':' characters
(add-hook 'fill-no-break-predicate 'fill-french-nobreak-p)

;; Display matching parenthesis
;; http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Show the matching parenthseis when it is offscreen
;; http://www.emacswiki.org/emacs/ShowParenMode#toc1
(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
echo area. Has no effect if the character before point is not of
the syntax class ')'."
  (interactive)
  (let ((matching-text nil))
    ;; Only call `blink-matching-open' if the character before point
    ;; is a close parentheses type character. Otherwise, there's not
    ;; really any point, and `blink-matching-open' would just echo
    ;; "Mismatched parentheses", which gets really annoying.
    (if (char-equal (char-syntax (char-before (point))) ?\))
(setq matching-text (blink-matching-open)))
    (if (not (null matching-text))
(message matching-text))))

(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#def")

;; Highlight current line
;; http://www.emacsblog.org/2007/04/09/highlight-the-current-line/
;; (global-hl-line-mode 1)

;; Active region becomes the window selection
;;(setq select-active-region t)
;; To customize the background color
;;(set-face-background 'hl-line "#111")

;; ediff window setup
;; - don't open a new frame for the control buffer
;; - split horizontally if the current frame is wide enough
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function (lambda (&optional arg)
(if (> (frame-width) 150)
(split-window-horizontally arg)
(split-window-vertically arg))))
;; - restore window configuration when quitting ediff
(add-hook 'ediff-load-hook
(lambda ()
(add-hook 'ediff-before-setup-hook
(lambda ()
(setq ediff-saved-window-configuration (current-window-configuration))))
(let ((restore-window-configuration
(lambda ()
(set-window-configuration ediff-saved-window-configuration))))
(add-hook 'ediff-quit-hook restore-window-configuration 'append)
(add-hook 'ediff-suspend-hook restore-window-configuration 'append))))

;; Remember the last visited line in a file
(setq-default save-place t)
(require 'saveplace)
(setq save-place-file "~/.config/emacs/places")

;; ;; Save opened files and other stuff
;; ;; http://www.xsteve.at/prg/emacs/power-user-tips.html
;; (setq desktop-save t
;;       desktop-load-locked-desktop t
;;       desktop-path '("." "~/.config/emacs"))
;; (desktop-save-mode 1)
;; (setq desktop-globals-to-save
;;       (append '((extended-command-history . 30)
;;                 (file-name-history . 100)
;;                 (grep-history . 30)
;;                 (compile-history . 30)
;;                 (minibuffer-history . 50)
;;                 (query-replace-history . 60)
;;                 (read-expression-history . 60)
;;                 (regexp-history . 60)
;;                 (regexp-search-ring . 20)
;;                 (search-ring . 20)
;;                 (shell-command-history . 50)
;;                 tags-file-name
;;                 register-alist)))
;; ;; http://www.emacswiki.org/emacs/DeskTop
;; (add-hook 'auto-save-hook 'desktop-save-in-desktop-dir)

;; Abort the minibuffer when using the mouse
;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (>= (recursion-depth) 1)
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Don't ask if I want to kill a buffer with a live process attached to it
;; http://www.masteringemacs.org/articles/2010/11/14/disabling-prompts-emacs/
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
kill-buffer-query-functions))

;; Automagically make scripts executable
;; http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ido-mode for better buffer switching, file selection, etc.
(require 'ido)
(ido-mode 1)
(setq ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

;; Wait a very little bit before fontifying buffers
;; http://tsengf.blogspot.fr/2012/11/slow-scrolling-speed-in-emacs.html
(setq jit-lock-defer-time 0.05)

;; recentf
(require 'recentf)    ;; save recently used files
(setq
 recentf-save-file "~/.emacs.d/cache/recentf"
 recentf-max-saved-items 100     ;; max save 100
 recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode t)                  ;; turn it on


(setq backup-directory-alist '(("." . "~/.emacs.d/cache"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(require 'uniquify) ;; make buffer names more unique
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")

;; overrride the default function....
(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))

;; slick-copy: make copy-past a bit more intelligent
;; from: http://www.emacswiki.org/emacs/SlickCopy
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(setq ;; scrolling
 scroll-margin 0                        ;; do smooth scrolling, ...
 scroll-conservatively 100000           ;; ... the defaults ...
 scroll-up-aggressively 0               ;; ... are very ...
 scroll-down-aggressively 0             ;; ... annoying
 scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v 

(put 'narrow-to-region 'disabled nil)    ;; enable...
(put 'erase-buffer 'disabled nil)        ;; ... useful things
(file-name-shadow-mode t)                ;; be smart about filenames in mbuf

(load-library "iso-transl")
