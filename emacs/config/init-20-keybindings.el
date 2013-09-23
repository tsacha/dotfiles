;; Remappage Bépo - Raccourcis ± ergonomiques
;; Inversion de Ctrl et Alt sur les opérations
;; courantes

;; Déplacements sur la partie gauche de clavier
;; La partie droite est blindée de mines : C-s, C-m, C-j
;; À gauche C-k semble poser quelques problèmes selon les contextes

(global-set-key [?\M-a] 'backward-char)
(global-set-key [?\M-i] 'next-line)
(global-set-key [?\M-u] 'previous-line)
(global-set-key [?\M-e] 'forward-char)

(global-set-key [?\C-a] 'backward-word)
(global-set-key [?\C-e] 'forward-word)

(global-set-key [?\M-b] 'beginning-of-line)
(global-set-key [?\M-o] 'end-of-line)

(global-set-key [?\M-.] 'scroll-up)
(global-set-key [?\M-y] 'scroll-down)

(global-set-key [?\C-b] 'beginning-of-buffer)
(global-set-key [?\C-w] 'end-of-buffer)

(global-set-key [?\M-é] 'backward-paragraph)
(global-set-key [?\M-p] 'forward-paragraph)

(global-set-key [?\M-c] 'kill-line)
(global-set-key [?\C-c] 'kill-sentence)
(global-set-key [?\M-t] 'delete-char)
(global-set-key [?\C-t] 'kill-word)

(global-set-key [?\M-v] 'undo-tree-undo)
(global-set-key [?\M-d] 'undo-tree-redo)
(global-set-key [?\C-v] 'undo-tree-switch-branch)
(global-set-key [?\C-d] 'undo-tree-undo)

(global-set-key [?\M-g] 'save-buffers-kill-terminal)
(global-set-key [?\C-q] 'find-file)
(global-set-key [?\M-q] 'save-buffer)

(global-set-key [?\M-+] 'set-mark-command)
(global-set-key [?\M--] 'mark-whole-buffer)
(global-set-key [?\M-j] 'kill-ring-save)
(global-set-key [?\M-n] 'yank)
(global-set-key [?\M-m] 'yank-pop)
(global-set-key [?\M-f] 'kill-region)

(global-set-key [?\M-,] 'universal-argument)
(global-set-key [?\M-h] 'goto-line)
(global-set-key [?\M-z] 'shell-command)

(global-set-key [?\M-$] 'other-window)
(global-set-key [?\M-"] 'split-window-right)
(global-set-key [?\M-«] 'split-window-below)
(global-set-key [?\M-»] 'delete-other-windows)
(global-set-key [?\M-(] 'delete-other-windows)
(global-set-key [?\M-)] 'delete-window)

(define-key minibuffer-local-map (kbd "C-p") 'next-complete-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'yank)
