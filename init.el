;;  init.el --- Emacs user-init-file
;;; Commentary:
;; TODO: Explore byte compiling this file to improve startup time
;; TODO: Prior config had a custom insert to avoid insert on read-only buffers.
;; TODO: Quoting lambdas doesn't let compiler to compile them, it seems is not necessary.
;; TODO: Zshell is not working right.
;; TODO: Use evil-window-map to define the C-w mappings.
;; TODO: Use evil-add-hjkl-bindings KEYMAP STATE &rest BINDINGS to make evil
;;       work on compilation-mode-map and Info-mode-map.
;; TODO: :general :keymaps is not working, an example is dired-mode-map
;;       navigation keys.
;; TODO: helm-projectile is not being loaded until you call some function in
;;       projectile wich seems now use-package's :command works diferently.
;; TODO: Manage packages with straight.el to have reproducible configurations.
;; TODO: Make that if cursor is at the end M-v is paste after, otherwise is paste before.

;;; Code:
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold 800000)))


;; ===================================================
;; EARLY SETTINGS
;; ===================================================

;; Remove all GUI stuff
(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(tool-bar-mode -1)

(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)

;; (unless package-archive-contents
;; ;;   (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))

(require 'bind-key)

;; ===================================================
;; VARIABLES
;; ===================================================
(defvar zz-motion-up "h")
(defvar zz-motion-down "k")
(defvar zz-motion-left "j")
(defvar zz-motion-right "l")

;; ===================================================
;; FUNCTIONS
;; ===================================================
(defun zz-scroll-half-page (direction)
  "Scroll half page up if DIRECTION is non-nil,other wise will scroll half page down."
  (let ((opos (cdr (nth 6 (posn-at-point)))))
    ;; opos = original position line relative to window
    (move-to-window-line nil)     ;; Move cursor to middle line
    (if direction
        (recenter-top-bottom -1)  ;; Current line becomes last
      (recenter-top-bottom 0))    ;; Current line becomes first
    (move-to-window-line opos)))

(defun zz-scroll-half-page-down ()
  "Scrolls exactly half page down keeping cursor position."
  (interactive)
  (zz-scroll-half-page nil))

(defun zz-scroll-half-page-up ()
  "Scrolls exactly half page up keeping cursor position."
  (interactive)
  (zz-scroll-half-page t))

(defun zz-on-asus ()
  "Return non-nil if we are on asus monitor."
  (and (= 2048 (display-pixel-width))
       (= 1152 (display-pixel-height))))

(defun zz-on-pro ()
  "Return non-nil if we are on macbook pro retina display."
  (and (= 1280 (display-pixel-width))
       (= 800 (display-pixel-height))))

(defun zz-toggle-projectile-dired ()
  "Toggle projectile-dired buffer."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (evil-delete-buffer (current-buffer))
    (if (projectile-project-p)
        (projectile-dired)
      (dired (file-name-directory buffer-file-name)))))

(defun zz-find-file ()
  "When in a project use projectile to find a file otherwise use helm."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (helm-for-files)))

(defun zz-toggle-dired ()
  "Toggle dired buffer."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (evil-delete-buffer (current-buffer))
    (dired (file-name-directory (or buffer-file-name "~/")))))

(defun zz-preferences ()
  "Open the `user-init-file'."
  (interactive)
  (find-file user-init-file))

(defun zz-kill-buffer ()
  "Alias for killing current buffer."
  (interactive)
  (kill-buffer nil))

(defun zz-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (message "clear"))

(defun zz-evil-window-delete-or-die ()
  "Delete the selected window.  If this is the last one, exit Emacs."
  (interactive)
  (condition-case nil
      (evil-window-delete)
    (error (condition-case nil
               (delete-frame)
             (error (evil-quit))))))

(defun zz-minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun zz-dired-sort-directories-first ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(defun zz-dired-up-directory ()
  "Go up one level in the directory structure reusing dired buffer."
  (interactive)
  (find-alternate-file ".."))

(defun zz-save-to-escape ()
  "First save the buffer and then escape from insert."
  (interactive)
  (save-buffer)
  (evil-normal-state))

(defun zz-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun zz-git-gutter-stage-hunk ()
  "Don't ask for confirmation when staging a hunk."
  (interactive)
  (setq git-gutter:ask-p nil)
  (git-gutter:stage-hunk)
  (setq git-gutter:ask-p t))

(defun zz-file-stats ()
  "Gives the filename, current line and column of point."
  (interactive)
  (let* (
         (cursor-position (what-cursor-position))
         (line (what-line))
         (percent ((lambda ()
                     (string-match "\\([0-9]+\\)%" cursor-position)
                     (match-string 1 cursor-position))))
         (column ((lambda ()
                    (string-match "column=\\([0-9]+\\)" cursor-position)
                    (match-string 1 cursor-position)))))
    (message (format "%S %s Column %s | --%s%%%%--" buffer-file-name line column percent))))

(defun zz-scroll-line-to-quarter ()
  "Scrolls windows so that the current line ends at ~25% of the window."
  (interactive)
  (evil-scroll-line-to-top (- (line-number-at-pos) 5))
  (evil-next-visual-line 5))

(defun zz-eshell-current-dir ()
  "Open eshell in the current dir or use projectile-run-eshell in a project."
  (interactive)
  (if (projectile-project-p)
      (projectile-run-eshell)
    (eshell)))

(defun zz-zshell-current-dir ()
  "Open zshell in the current dir."
  (interactive)
  (if (projectile-project-p)
      (projectile-run-term "/usr/local/bin/zsh")
    (ansi-term "/usr/local/bin/zsh")))

(defun zz-shell-insert ()
  "When entering insert mode do it at the prompt line."
  (interactive)
  (let* ((curline (line-number-at-pos))
         (endline (line-number-at-pos (point-max))))
    (if (= curline endline)
        (if (not (eobp))
            (let ((plist (text-properties-at (point)))
                  (next-change (or (next-property-change (point) (current-buffer))
                                   (point-max))))
              (if (plist-get plist 'read-only)
                  (goto-char next-change))))
      (goto-char (point-max)))))

(defun zz-eshell-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (recenter-top-bottom 0))

(defun zz-evil-select-pasted ()
  "Visually select last pasted text."
  (interactive)
  (evil-goto-mark ?[)
  (evil-visual-char)
  (evil-goto-mark ?]))

(defun flyspell-goto-previous-error (arg)
  "Go to ARG previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        ;; (backward-word 1)
        (evil-backward-word-begin 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))))))

;; ===================================================
;; PACKAGES
;; ===================================================

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

(use-package diminish
  :ensure t)

(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-define-key :states 'motion "M-," 'zz-preferences)
  (general-define-key :states 'normal :prefix "SPC"
                      "ff" 'find-file
                      "hk" 'describe-key
                      "hm" 'describe-mode
                      "hb" 'describe-bindings
                      "hp" 'describe-package
                      "hv" 'describe-variable
                      "hf" 'describe-function
                      "ha" 'apropos-command
                      "hd" 'apropos-documentation
                      "hi" 'info)
  (general-define-key :keymaps 'normal :prefix "SPC"
                      "ln" 'linum-mode
                      "ta" 'align-regexp)
  (general-define-key :keymaps '(package-menu-mode-map compilation-mode-map)
                      "C-u" 'evil-scroll-up
                      "C-d" 'evil-scroll-down
                      "C-f" 'evil-scroll-page-down
                      "C-b" 'evil-scroll-page-up
                      ;; zz-motion-up 'evil-previous-line
                      ;; zz-motion-down 'evil-next-line
                      "/" 'evil-search-forward
                      "?" 'evil-search-backward
                      "n" 'evil-search-next
                      "N" 'evil-search-previous))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode)
  (fset 'evil-visual-update-x-selection 'ignore)
  ;; Fix scrolling (like in vim)
  (evil-define-motion evil-scroll-up (count)
    "Fixes the half-page scroll up to behave as in VIM"
    :repeat motion
    :type line
    (zz-scroll-half-page t))
  (evil-define-motion evil-scroll-down (count)
    "Fixes the half-page scroll down to behave as in VIM"
    :repeat motion
    :type line
    (zz-scroll-half-page nil))
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "full[screen]" 'toggle-frame-fullscreen))

  :general
  (:keymaps 'evil-ex-completion-map
            "C-w" 'backward-kill-word)
  (:states 'motion
            zz-motion-up 'evil-previous-line
            zz-motion-down 'evil-next-line
            zz-motion-left 'evil-backward-char
            zz-motion-right 'evil-forward-char)
  (:keymaps '(minibuffer-local-map minibuffer-local-ns-map
  minibuffer-local-completion-map minibuffer-local-must-match-map
  minibuffer-local-isearch-map)
            "<escape>" 'zz-minibuffer-keyboard-quit)
  (:states 'motion
           "<escape>" 'keyboard-quit
           "ga" 'zz-file-stats)
  (:keymaps 'normal
            (concat "C-w " zz-motion-down) 'evil-window-down
            (concat "C-w " zz-motion-up) 'evil-window-up
            (concat "C-w " zz-motion-left) 'evil-window-left

            "M-v" 'evil-paste-after
            "M-s" 'evil-write
            "C-s" 'evil-write

            "gV" 'zz-evil-select-pasted
            "zv" 'zz-scroll-line-to-quarter
            "RET" 'newline

            ;; Buffer navigation
            "gb" 'evil-buffer
            "M-{" 'evil-prev-buffer
            "M-}" 'evil-next-buffer
            "C-a k" 'zz-kill-buffer
            "C-a c" 'zz-kill-other-buffers
            "C-a d" 'zz-evil-window-delete-or-die

            ;; Frames
            "C-w x" 'other-frame
            "C-w f" 'make-frame-command)

  (:keymaps 'insert
            "M-s" 'zz-save-to-escape
            "C-s" 'zz-save-to-escape
            "M-v" 'evil-paste-after)

  (:keymaps 'visual
            "M-c" 'evil-yank
            "M-v" 'evil-visual-paste))

(use-package key-chord
  :ensure t
  :demand t
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2)
  :general
  (:keymaps 'insert
            (general-chord "uh") 'evil-normal-state))

(use-package magit
  :ensure t
  :commands (magit-after-revert-hook magit-not-reverted-hook)
  :init
  (add-hook 'git-gutter:update-hooks 'magit-after-revert-hook)
  (add-hook 'git-gutter:update-hooks 'magit-not-reverted-hook)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-save-repository-buffers nil)
  :general
  (:states 'normal
           :prefix "SPC"
           "gs" 'magit-status
           "go" 'zz-magit-checkout
           "gl" 'magit-log-current
           "gb" 'magit-blame
           "gm" 'magit-merge
           "gpl" 'magit-pull
           "gps" 'magit-push
           "mgr" 'magit-ediff-resolve))

(use-package evil-magit
  :ensure t
  :after magit
  :init
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  :config
  (general-evil-define-key 'motion magit-mode-map
    zz-motion-down 'evil-next-visual-line
    zz-motion-up 'evil-previous-visual-line
    (concat "C-w " zz-motion-down) 'evil-window-down
    (concat "C-w " zz-motion-up) 'evil-window-up
    (concat "C-w " zz-motion-left) 'evil-window-left))

(use-package evil-surround
  :ensure t
  :defer 2
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :defer t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-snipe
  :ensure t
  :diminish snipe
  :defer 2
  :commands turn-off-evil-snipe-override-mode
  :init
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  (setq evil-snipe-spillover-scope 'whole-visible
        evil-snipe-show-prompt nil))

(use-package evil-visualstar
  :ensure t
  :defer 1
  :config
  (global-evil-visualstar-mode))

(use-package evil-embrace
  :ensure t
  :defer t
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-lispy
  :ensure t
  :commands evil-lispy-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode))

(use-package drag-stuff
  :ensure t
  :commands (drag-stuff-up drag-stuff-down)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-global-mode t)

  :general
  (:keymaps 'global
            (concat "M-" zz-motion-up) #'drag-stuff-up
            (concat "M-" zz-motion-down) #'drag-stuff-down))

(use-package keyfreq
  :ensure t
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          abort-recursive-edit
          forward-char
          backward-char
          previous-line
          evil-next-visual-line
          evil-previous-visual-line
          next-line
          evil-write
          evil-forward-char
          evil-forward-word-begin
          evil-scroll-donw
          evil-backward-char
          evil-next-line
          evil-scroll-up
          save-buffers-kill-terminal
          save-buffers-kill-emacs))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package flyspell
  :ensure nil
  :commands (flyspell-mode flyspell-buffer git-commit-turn-on-flyspell)
  :init
  (add-hook 'org-mode-hook '(lambda () (flyspell-mode) (flyspell-buffer)))
  (add-hook 'markdown-mode-hook '(lambda () (flyspell-mode) (flyspell-buffer)))
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  :config
  (flyspell-mode 1)
  :general
  (:keymaps 'normal
            "]s" 'flyspell-goto-next-error
            "[s" 'flyspell-goto-previous-error)
  (:keymaps 'normal
            :prefix "SPC"
            "fl" 'flyspell-auto-correct-previous-word
            "ss" '(lambda () (interactive) (ispell-change-dictionary "espanol") (flyspell-buffer))
            "se" '(lambda () (interactive) (ispell-change-dictionary "english") (flyspell-buffer))))

(use-package projectile
  :ensure t
  :commands (projectile-project-p)
  :diminish projectile-mode
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm)

  :general
  (:keymaps 'normal
            :prefix "SPC"
            "tr" 'zz-toggle-projectile-dired))

(use-package cask-mode
  :ensure t)

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (require 'helm-config)
  (helm-mode 1)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t)

  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "ls" 'helm-buffers-list))

  :general
  ("M-x" 'helm-M-x)
  (:keymaps 'helm-map
            (concat "C-" zz-motion-up) 'helm-previous-line
            (concat "C-" zz-motion-down) 'helm-next-line
            "TAB" 'helm-next-source)
  (:states 'normal
            :prefix "SPC"
            "mx" 'helm-M-x
            "ls" 'helm-buffers-list))

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile-switch-project zz-find-file)
  :config
  (require 'tramp)
  (helm-projectile-on)
  :general
  ("M-P" 'helm-projectile-switch-project
   "M-p" 'zz-find-file)
  (:states 'normal
            "C-p" 'zz-find-file))

(use-package helm-flyspell
  :ensure t
  :general
  (:keymaps 'normal
            :prefix "SPC"
            "fs" 'helm-flyspell-correct))

(use-package neotree
  :ensure t
  :config
  (setq neo-window-width 30)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (add-hook 'neotree-mode-hook
            '(lambda ()
               (interactive)
               (setq buffer-face-mode-face '(:family "Hack" :height 130 :width semi-condensed))
               (buffer-face-mode)))

  :general
  (:states 'normal "M-1" 'neotree-toggle)
  (:keymaps 'normal
            :prefix "SPC"
            "tl" 'neotree-toggle)
  (:states 'normal
           :keymaps 'neotree-mode-map
           "o" 'neotree-enter
           "md" 'neotree-delete-node
           "ma" 'neotree-create-node
           "mm" 'neotree-rename-node
           "C" 'neotree-change-root
           "R" 'neotree-refresh
           "TAB" 'neotree-enter
           "RET" 'neotree-enter
           "?" 'describe-mode
           "A" 'neotree-stretch-toggle
           "H" 'neotree-hidden-file-toggle
           "n" 'next-line
           "p" 'previous-line
           "q" 'neotree-hide
           "u" 'neotree-select-up-node))

(use-package buffer-move
  :ensure t
  :config
  (setq buffer-move-stay-after-swap t)
  :general
  (:keymaps 'normal
            (concat "C-w " (upcase zz-motion-up)) 'buf-move-up
            (concat "C-w " (upcase zz-motion-down)) 'buf-move-down
            (concat "C-w " (upcase zz-motion-left)) 'buf-move-left
            (concat "C-w " (upcase zz-motion-right)) 'buf-move-right))

(use-package centered-window-mode
  :ensure t
  :config
  (when (zz-on-asus)
    (setq cwm-centered-window-width 100))
  (when (zz-on-pro)
    (setq cwm-centered-window-width 90))
  :general
  (:keymaps 'normal
            :prefix "SPC"
            "cw" 'centered-window-mode))

(use-package yasnippet
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all))

(use-package flymake
  :ensure nil
  :disabled t)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :commands global-flycheck-mode
  :init
  (add-hook 'prog-mode-hook 'global-flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 5
        flycheck-check-syntax-automatically '(save idle-change)
        flycheck-indication-mode nil)
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "fly[check]" 'flycheck-list-errors))

  :general
  (:keymaps 'normal
            :prefix "["
            "e" 'flycheck-previous-error)
  (:keymaps 'normal
            :prefix "]"
            "e" 'flycheck-next-error))

(use-package smartparens-config
  :ensure smartparens
  :defer 1
  :commands smartparens-mode
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :config
  (sp-with-modes 'emacs-lisp-mode
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)))
  (sp-with-modes 'markdown-mode
    (sp-local-pair "_" "_")
    (sp-local-pair "**" "**")
    (sp-local-pair "~~" "~~"))

  ;; For modes use: (sp-local-pair '(c-mode) "{" nil :post-handlers '((zz-newline-and-enter "RET")))
  ;; Indent a new line when pressing RET after pair insertion
  (sp-pair "{" nil :post-handlers '((zz-newline-and-enter-sexp "RET")))
  (sp-pair "(" nil :post-handlers '((zz-newline-and-enter-sexp "RET")))
  (sp-pair "[" nil :post-handlers '((zz-newline-and-enter-sexp "RET"))))

(use-package expand-region
  :ensure t
  :general
  (:keymaps 'visual
            "v" 'er/expand-region
            "V" 'er/contract-region))

(use-package web-mode
  :ensure t
  :mode ("\\.blade\\.php\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'" "\\.css\\'")
  :config
  (setq web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-engines-alist '(("django" . ".*/python/django/.*\\.html\\'")))
  :general
  (:keymaps '(normal web-mode-map)
            :prefix "SPC" "rr" 'browse-url-of-file))

(use-package php-mode
  :ensure t
  :defer t)

(use-package phpunit
  :ensure t
  :after php-mode
  :general
  (:keymaps 'normal
            :prefix "SPC"
            "rr" 'phpunit-current-test
            "rt" 'phpunit-current-class
            "ra" 'phpunit-current-project))

(use-package rbenv
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'global-rbenv-mode)
  :config
  (setq rbenv-show-active-ruby-in-modeline nil))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (setq-default js2-basic-offset 2)
  (add-to-list 'load-path "~/.config/yarn/global/node_modules/tern/emacs/tern.el")
  (autoload 'tern-mode "tern.el" nil t)
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode t)
                             (js2-imenu-extras-mode))))

(use-package json-mode
  :ensure t
  :defer t)

(use-package feature-mode
  :ensure t
  :mode "\\.feature\\'")

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  :config
  (setq markdown-gfm-use-electric-backquote nil))

(use-package octave
  :ensure nil
  :mode ("\\.m$" . octave-mode))

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package go-mode
  :ensure t
  :defer t
  :config
  (add-hook 'go-mode-hook
            '(lambda ()
               (add-hook 'before-save-hook
                         '(lambda ()
                            (gofmt-before-save)
                            (go-remove-unused-imports))
                         nil
                         'local))))

(use-package go-eldoc
  :ensure t
  :commands go-eldoc-setup
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))


(use-package company
  :ensure t
  :pin melpa
  :defer t
  :config
  (global-company-mode)
  (company-tng-configure-default)
  (setq company-dabbrev-downcase 0
        company-idle-delay 0.2))

(use-package company-web
  :ensure t
  :init
  (add-hook 'web-mode-hook (lambda ()
                             (set (make-local-variable 'company-backends) '(company-web-html))
                             (company-mode t))))

(use-package company-tern
  :ensure t
  :init
  (add-hook 'js2-mode-hook '(lambda ()
                              (set (make-local-variable 'company-backends) '(company-tern))
                              (company-mode t))))

(use-package company-go
  :ensure t
  :commands company-mode
  :init
  (add-hook 'go-mode-hook '(lambda ()
                             (set (make-local-variable 'company-backends) '(company-go))
                             (company-mode t))))

(use-package vimish-fold
  :ensure t
  :defer 2
  :config
  (vimish-fold-global-mode 1))

(use-package evil-vimish-fold
  :ensure t
  :after vimish-fold
  :config
  (evil-vimish-fold-mode 1)
  :general
  (:states '(normal motion) :keymaps 'evil-vimish-fold-mode-map
           (concat "z" zz-motion-up) 'evil-vimish-fold/previous-fold
           (concat "z" zz-motion-down) 'evil-vimish-fold/next-fold))

(use-package org
  :ensure t
  :pin org
  :config
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (setq org-log-done t
        org-ellipsis " ⤵"
        org-src-fontify-natively t
        org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE"))
        org-agenda-files (list "~/Documents/org/free.org"
                               "~/Documents/org/paid.org"
                               "~/Documents/org/todo.org"))
  :general
  (:keymaps 'normal
            :prefix "SPC"
            "oa" 'org-agenda
            "ot" 'org-todo)
  (:keymaps 'org-mode-map
            :states 'normal
            :prefix "SPC"
            "j" 'evil-join)
  (:keymaps 'org-mode-map
            :states '(insert normal)
            (concat "M-" zz-motion-up) 'org-metaup
            (concat "M-" zz-motion-down) 'org-metadown
            (concat "M-" zz-motion-left) 'org-metaleft
            (concat "M-" zz-motion-right) 'org-metaright)
  (:keymaps 'org-mode-map
            :states 'normal
            (upcase zz-motion-up) 'org-shiftup
            (upcase zz-motion-down) 'org-shiftdown
            (upcase zz-motion-right) 'org-shiftright
            (upcase zz-motion-left) 'org-shift-left)
  (:keymaps 'org-agenda-mode-map
            :states '(insert emacs)
            (concat "C-" zz-motion-up) 'org-agenda-next-line
            (concat "C-" zz-motion-down) 'org-agenda-previous-line))

(use-package org-bullets
  :ensure t
  :after org
  :config
  (org-bullets-mode 1))

(use-package ox-gfm
  :ensure t
  :after org)

(use-package ox-reveal
  :ensure t
  :after org
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
        org-reveal-mathjax t))

(use-package spaceline-config
  :ensure spaceline
  :commands spaceline-spacemacs-theme
  :init
  (add-hook 'after-init-hook 'spaceline-spacemacs-theme)
  :config
  (spaceline-helm-mode)
  (setq powerline-default-separator 'wave)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-toggle-minor-modes-off))

(use-package window-numbering
  :ensure t
  :after spaceline
  :config
  (window-numbering-mode t)
  (setq spaceline-window-numbers-unicode t))

(use-package eyebrowse
  :ensure t
  :after spaceline
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys)
  (setq eyebrowse-wrap-around t
        eyebrowse-new-workspace t
        spaceline-workspace-numbers-unicode t)
  :general
  (:states 'normal :keymaps '(messages-buffer-mode-map dired-mode-map)
           "gt" 'eyebrowse-next-window-config)
  (:keymaps 'normal :prefix "SPC"
            "t0" 'eyebrowse-switch-to-window-config-0
            "t1" 'eyebrowse-switch-to-window-config-1
            "t2" 'eyebrowse-switch-to-window-config-2
            "t3" 'eyebrowse-switch-to-window-config-3
            "t4" 'eyebrowse-switch-to-window-config-4
            "t5" 'eyebrowse-switch-to-window-config-5
            "t6" 'eyebrowse-switch-to-window-config-6
            "t7" 'eyebrowse-switch-to-window-config-7
            "t8" 'eyebrowse-switch-to-window-config-8
            "t9" 'eyebrowse-switch-to-window-config-9))

(use-package nyan-mode
  :ensure t
  :if (display-graphic-p)
  :after spaceline
  :config
  (nyan-mode t)
  (setq nyan-animate-nyancat t
        nyan-bar-length 22))

(use-package fancy-battery
  :ensure t
  :after spaceline
  :config
  (fancy-battery-mode)
  (setq fancy-battery-show-percentage t))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init
  (setq git-gutter:hide-gutter t
        git-gutter:modified-sign " ~"
        git-gutter:added-sign " +"
        git-gutter:deleted-sign " -"
        git-gutter:window-width 3)
  (global-git-gutter-mode +1)
  :config
  (set-face-foreground 'git-gutter:modified "#b58900")
  (set-face-foreground 'git-gutter:added "#859900")
  (set-face-foreground 'git-gutter:deleted "#dc322f")
  (set-face-font 'git-gutter:modified "Menlo")
  (set-face-font 'git-gutter:added "Menlo")
  (set-face-font 'git-gutter:deleted "Menlo")
  :general
  (:keymaps 'normal
            "]h" 'git-gutter:next-hunk
            "[h" 'git-gutter:previous-hunk)
  (:keymaps 'normal :prefix "SPC"
            "ga" 'zz-git-gutter-stage-hunk
            "gr" 'git-gutter:revert-hunk))

(use-package evil-anzu
  :ensure t
  :after evil
  :config
  (setq anzu-cons-mode-line-p nil))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :commands all-the-icons-dired-mode
  :after all-the-icons
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package dockerfile-mode
  :ensure t)

(use-package highlight-chars
  :ensure t
  :commands hc-highlight-tabs
  :init
  (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
  :config
  (set-face-attribute 'hc-tab nil :background nil :foreground "#586e75"))

(use-package disable-mouse
  :ensure t
  :defer t
  :config
  (global-disable-mouse-mode))

(use-package pyvenv
  :ensure t
  :config
  (setq pyvenv-virtualenvwrapper-python
        (or (getenv "VIRTUALENVWRAPPER_PYTHON")
            (executable-find "python3"))))

(use-package elpy
  :ensure t
  :defer t
  :commands (flycheck-mode elpy-format-code)
  :init
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook
            '(lambda ()
               (add-hook 'before-save-hook '(lambda () (elpy-format-code))
                         nil
                         'local)))
  :config
  (elpy-enable)
  (elpy-use-ipython)
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-python-command "python3"
        elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (delete `elpy-module-highlight-indentation elpy-modules))

(use-package seti-theme :ensure t :disabled)
(use-package planet-theme :ensure t :disabled)
(use-package flatui-theme :ensure t :disabled)
(use-package base16-theme :ensure t :disabled)
(use-package molokai-theme :ensure t :disabled)
(use-package gruvbox-theme :ensure t :disabled)
(use-package dracula-theme :ensure t :disabled)
(use-package oceanic-theme :ensure t :disabled)
(use-package material-theme :ensure t :disabled)
(use-package twilight-theme :ensure t :disabled)
(use-package solarized-theme :ensure t :disabled)
(use-package soft-stone-theme :ensure t :disabled)
(use-package apropospriate-theme :ensure t :disabled)
(use-package twilight-bright-theme :ensure t :disabled)
(use-package twilight-anti-bright-theme :ensure t :disabled)
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (load-theme 'sanityinc-tomorrow-bright t))

(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :commands whitespace-mode
  :init
  (add-hook 'prog-mode-hook '(lambda ()
                               (whitespace-mode 0)
                               (setq whitespace-line-column 80)
                               (whitespace-mode 1)))
  (add-hook 'python-mode-hook '(lambda ()
                                 (whitespace-mode 0)
                                 (setq whitespace-line-column 79)
                                 (whitespace-mode 1)))
  (add-hook 'web-mode-hook '(lambda ()
                              (whitespace-mode 0)
                              (setq whitespace-line-column 101)
                              (whitespace-mode 1)))
  :config
  (setq whitespace-style '(face trailing tab-mark lines-tail)))

(use-package simple
  :ensure nil
  :diminish auto-fill-function
  :commands (turn-on-auto-fill set-fill-column)
  :init
  (add-hook 'markdown-mode-hook (lambda () (turn-on-auto-fill) (set-fill-column 80)))
  (add-hook 'org-mode-hook (lambda () (turn-on-auto-fill) (set-fill-column 80)))
  (add-hook 'prog-mode-hook (lambda() (turn-on-auto-fill) (set-fill-column 80)))
  (add-hook 'python-mode-hook (lambda() (turn-on-auto-fill) (set-fill-column 79))))

(use-package dired
  :defer t
  :commands (auto-revert-mode zz-dired-sort-directories-first)
  :init
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (add-hook 'dired-after-readin-hook 'zz-dired-sort-directories-first)
  :config
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)
  (evil-define-key 'normal dired-mode-map
    zz-motion-left nil
    zz-motion-right nil
    zz-motion-up 'dired-previous-line
    zz-motion-down 'dired-next-line)
  :general
  (:keymaps 'normal
            :prefix "SPC"
            "tt" 'zz-toggle-dired)
  ;; (:keymaps 'dired-mode-map
  ;;           zz-motion-up 'dired-previous-line
  ;;           zz-motion-down 'dired-next-line
  ;;           "o" 'dired-find-alternate-file
  ;;           "RET" 'dired-find-alternate-file
  ;;           "u" 'zz-dired-up-directory
  ;;           "C-r" 'revert-buffer
  ;;           "C-p" 'zz-find-file
  ;;           "r" 'dired-do-redisplay
  ;;           ;; "gb" 'evil-buffer
  ;;           "M-{" 'evil-prev-buffer
  ;;           "M-}" 'evil-next-buffer
  ;;           "mc" 'dired-do-copy
  ;;           "mm" 'dired-do-rename
  ;;           "ma" 'dired-create-directory
  ;;           "md" 'dired-do-delete)
  )

(use-package dired-subtree
  :ensure t
  :general
  (:states 'normal
           :keymaps 'dired-mode-map
           "<tab>" 'dired-subtree-toggle
           "<backtab>" 'dired-subtree-cycle))

(use-package minibuffer
  :defer t
  :init
  ;; Don't garbage collect too often
  (add-hook 'minibuffer-setup-hook #'(lambda () (interactive) (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'minibuffer-exit-hook #'(lambda () (interactive) (setq gc-cons-threshold 800000)))
  :general
  (:keymaps 'minibuffer-local-map
            "C-p" 'previous-history-element
            "C-n" 'next-history-element
            "C-w" 'backward-kill-word
            "M-v" 'clipboard-yank
            "<escape>" 'keyboard-escape-quit))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode)
  :config
  (setq emmet-preview-default nil)
  :general
  (:states 'insert
           :prefix "C-e"
           :keymaps '(html-mode-map web-mode-map css-mode-map nxml-mode-map)
            "," 'emmet-expand-line))

(use-package help-mode
  :ensure nil
  :after evil
  :init
  (add-hook 'help-mode-hook
            '(lambda ()
               (evil-define-key 'motion help-mode-map (kbd "gb") 'evil-buffer)
               (evil-define-key 'motion help-mode-map (kbd zz-motion-left) 'evil-backward-char)
               (evil-define-key 'motion help-mode-map (kbd zz-motion-right) 'evil-forward-char)
               (evil-define-key 'motion help-mode-map (kbd zz-motion-down) 'evil-next-line)
               (evil-define-key 'motion help-mode-map (kbd zz-motion-up) 'evil-previous-line))))

(use-package eshell
  :ensure nil
  :init
  (add-hook 'eshell-mode-hook
            '(lambda ()
               (add-hook 'evil-insert-state-entry-hook 'zz-shell-insert nil t)
               (general-evil-define-key 'insert eshell-mode-map
                 "C-l" 'zz-eshell-clear-buffer)))
  :config
  (setq eshell-cmpl-ignore-case t)
  ;; Error (use-package): eshell :config: Symbol’s value as variable is void: eshell-output-filter-functions
  ;; (delete `eshell-postoutput-scroll-to-bottom eshell-output-filter-functions)
  :general
  (:keymaps 'normal :prefix "SPC"
            "sh" 'zz-eshell-current-dir))

(use-package term
  :ensure nil
  :init
  (add-hook 'term-mode-hook
            '(lambda () (add-hook 'evil-insert-state-entry-hook 'zz-shell-insert nil t)))
  :general
  (:keymaps 'normal :prefix "SPC"
            "zsh" 'zz-zshell-current-dir))

(use-package undo-tree
  :ensure nil
  :diminish undo-tree-mode)

(use-package htmlize
  :ensure t)

(use-package esup
  :ensure t)

(setq user-full-name "Julio César"
      user-mail-address "zzantares@gmail.com")

(setq backup-by-copying t   ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)   ; use versioned backups

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs-saves/" t)))

;; Use proper ls in emacs mac os
(setq insert-directory-program "/usr/local/bin/gls")

;; Can answer 'y' instead of 'yes' when emacs asks a question
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable sound on bell
(setq ring-bell-function 'ignore)

;;  Disable blink cursor
(blink-cursor-mode -1)

;; Show filename in the window title
(setq frame-title-format '("%b"))

;; Don't show the startup screen
(setq inhibit-startup-screen t)

;; Delete trailing spaces after saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(when (and (display-graphic-p) (equal system-type 'darwin))
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  (setq mac-pass-command-to-system nil))

;; Identation and line spaceing settings
(setq tab-stop-list (number-sequence 4 200 4))
(setq-default line-spacing 15
              indent-tabs-mode nil
              tab-width 4)

;; Window border separator
(set-face-attribute 'vertical-border nil :foreground "black")

;; Font settings
;; (set-face-attribute 'default nil :height 180 :family "Inconsolata")
(set-face-attribute 'default nil :height 160 :family "Consolas")
;; (set-face-attribute 'default nil :height 170 :family "Ubuntu Mono")
;; (set-face-attribute 'default nil :height 160 :family "Operator Mono")
;; (set-face-attribute 'default nil :height 150 :family "Fira Code")
;; (set-face-attribute 'default nil :height 150 :family "Hack")
;; (set-face-attribute 'default nil :height 150 :family "Monaco")
;; (set-face-attribute 'default nil :height 150 :family "Menlo")
;; (set-face-attribute 'default nil :height 160 :family "Roboto Mono")
;; (set-face-attribute 'default nil :height 170 :family "Fantasque Sans Mono")
;; (set-face-attribute 'default nil :height 160 :family "Fira Mono")

(if (display-graphic-p)
    (progn
      ;; Use Fira Code ligatures when this font is activated
      (let ((font-query (query-font (face-attribute 'default :font))))
        (when (string-match-p (regexp-quote "Fira Code") (elt font-query 0))
          (mac-auto-operator-composition-mode)))

      (when (zz-on-asus)
        ;; Obtener medidas con (frame-width) (frame-height)
        (add-to-list 'default-frame-alist '(left . 0))
        (add-to-list 'default-frame-alist '(top . 0))
        (add-to-list 'default-frame-alist '(width . 225))
        (add-to-list 'default-frame-alist '(height . 61)))
      (when (zz-on-pro)
        (add-to-list 'default-frame-alist '(left . 4))
        (add-to-list 'default-frame-alist '(top . 0))
        (add-to-list 'default-frame-alist '(width . 140))
        (add-to-list 'default-frame-alist '(height . 42)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "3a69621a68c2d3550a4c777ffc000e1ea66f5bc2f61112814c591e1bda3f5704" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "dfc9bdac772757f350c131fb79faa29f18651ef7bfe0c291d1a62051d11daa90" "ba33dae124cf799da2e77820ece54cfd2df0be68cfc413b90af29419b229c212" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "80930c775cef2a97f2305bae6737a1c736079fdcc62a6fdf7b55de669fbbcd13" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "a56a6bf2ecb2ce4fa79ba636d0a5cf81ad9320a988ec4e55441a16d66b0c10e0" "5a970147df34752ed45bfdf0729233abfc085d9673ae7e40210c5e2d8f624b08" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "350dc341799fbbb81e59d1e6fff2b2c8772d7000e352a5c070aa4317127eee94" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "446cc97923e30dec43f10573ac085e384975d8a0c55159464ea6ef001f4a16ba" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "2b6bd2ebad907ee42b3ffefa4831f348e3652ea8245570cdda67f0034f07db93" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "125fd2180e880802ae98b85f282b17f0aa8fa6cb9fc4f33d7fb19a38c40acef0" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (color-theme-sanityinc-tomorrow soft-stone emmet-mode dockerfile-mode all-the-icons evil-anzu fancy-battery nyan-mode eyebrowse window-numbering yaml-mode htmlize evil-vimish-fold vimish-fold esup dired base16-theme gruvbox-theme seti-theme solarized-theme twilight-theme flatui-theme oceanic-theme molokai-theme markdown-mode feature-mode json-mode js2-mode rbenv phpunit php-mode web-mode smartparens flycheck yasnippet centered-window-mode buffer-move neotree helm-flyspell helm-projectile helm cask-mode projectile flyspell-lazy keyfreq drag-stuff evil-lispy evil-embrace evil-visualstar evil-snipe evil-matchit evil-surround evil-magit magit key-chord evil general exec-path-from-shell use-package)))
 '(safe-local-variable-values
   (quote
    ((eval pyvenv-activate
           (concat
            (projectile-project-root)
            ".venv"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
