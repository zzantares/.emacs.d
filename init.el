;;  init.el --- Emacs user-init-file
;;; Commentary:
;; TODO: Explore byte compiling this file to improve startup time.
;; TODO: Prior config had a custom insert to avoid insert on read-only buffers.
;; TODO: Quoting lambdas doesn't let compiler to compile them, it seems is not necessary.
;; TODO: Zshell is not working right.
;; TODO: Use evil-window-map to define the C-w mappings.
;; TODO: Explore evil-want-* settings.
;; TODO: Make that if cursor is at the end M-v is paste after, otherwise is paste before.
;; TODO: Adopt the usage of use-package's :hook keyword.
;; TODO: Explore the org-mode features from https://github.com/dieggsy/dotfiles/tree/master/emacs.d
;; TODO: Org easy templates expansion not working with ox-reveal (https://github.com/yjwen/org-reveal/issues/323)
;; TODO: Add settings for disabling auto-save on .gpg files (https://www.reddit.com/r/emacs/comments/46lv2q/is_there_any_easy_way_to_make_org_files_password/d08j4fb/)
;; TODO: Fix the up and down motions while visually selecting a hunk in magit status mode.
;; TODO: In haskell-mode pressing Y freezes Emacs https://github.com/expez/evil-smartparens/issues/50
;; TODO: When oppening text files or markdown files buffer loads until spell check finishes.
;; TODO: Fix defer issues with centered-window-mode and hide-mode-line
;; TODO: Try out "sebastiencs/omnibox"
;; TODO: Use M-RET for fullscreen, maximized and normal window toggle?
;; TODO: How about using flyspell-prog-mode?
;; TODO: Move vanilla emacs configurations to the respective block (before org settings)
;; TODO: Dive in https://emacscast.org/ to find settings of org capture and ox-hugo
;; TODO: For PureScript https://github.com/kRITZCREEK/a-whole-new-world/blob/master/init.el#L360
;; TODO: Keybinding to kill the buffer and then close the window.
;; TODO: ':hook' can be used as a code block not a parenthesized expression?
;; TODO: What about https://github.com/dbordak/telephone-line for powerline replacement?
;; TODO: Add a function that does "vipgq" that is used frequently in org-mode.

;; ===================================================
;; NOTES & REMINDERS
;; ===================================================
;; => Before updating packages if all is working ok run straight-freeze-versions first.

;;(package-initialize)

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

;; Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t
      straight-vc-git-default-protocol 'ssh
      straight-vc-git-force-protocol t)

(straight-use-package 'use-package)

(setq use-package-verbose t
      use-package-always-defer t)

(eval-when-compile
  (require 'use-package))

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

(defun zz-toggle-projectile-dired ()
  "Toggle projectile-dired buffer."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (evil-delete-buffer (current-buffer))
    (if (projectile-project-p)
        (projectile-dired)
      (dired (file-name-directory buffer-file-name)))))

(defun zz-find-file ()
  "When in a project use projectile to find a file otherwise use counsel."
  (interactive)
  (if (projectile-project-p)
      (if (projectile-file-exists-p ".git")
          (counsel-git)
        (counsel-projectile-find-file))
    (counsel-find-file)))

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

(defun zz-magit-checkout (revision)
  "Check out a REVISION branch updating the mode line (reverts the buffer)."
  (interactive (list (magit-read-other-branch-or-commit "Checkout")))
  (magit-checkout revision)
  (revert-buffer t t))

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

(defun zz-quit-help-like-windows (&optional kill frame)
  "Quit all windows with help-like buffers.
Call `quit-windows-on' for every buffer named in
`zz-help-like-windows-name'.  The optional parameters KILL and FRAME
are just as in `quit-windows-on', except FRAME defaults to t (so
that only windows on the selected frame are considered).

Note that a nil value for FRAME cannot be distinguished from an
omitted parameter and will be ignored; use some other value if
you want to quit windows on all frames."
  (interactive)
  (let ((frame (or frame t))
        (zz-help-like-windows '(;; Ubiquitous help buffers
                                "*Help*"
                                "*Apropos*"
                                "*Messages*"
                                "*Completions*"
                                ;; Other general buffers
                                "*Command History*"
                                "*Compile-Log*"
                                "*disabled command*")))
    (dolist (name zz-help-like-windows)
      (ignore-errors
        (quit-windows-on name kill frame)))))

;; See: https://emacs.stackexchange.com/a/10233/12340
(defun zz-lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

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

(use-package no-littering
  :demand t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror))

(use-package exec-path-from-shell
  :defer 3
  :if (memq window-system '(mac ns x))
  :config
  (dolist (envvar '("GOPATH" "GOROOT"))
    (add-to-list 'exec-path-from-shell-variables envvar))
  (exec-path-from-shell-initialize))

(use-package diminish)

(use-package general
  :demand t
  :config
  (general-define-key :states 'motion "M-," 'zz-preferences)
  (general-define-key :states 'normal :prefix "SPC"
    "q" 'zz-quit-help-like-windows
    "hk" 'describe-key
    "hm" 'describe-mode
    "hb" 'describe-bindings
    "hp" 'describe-package
    "hv" 'describe-variable
    "hf" 'describe-function
    "ha" 'apropos-command
    "hd" 'apropos-documentation
    "hi" 'info)
  (general-define-key :keymaps '(normal visual) :prefix "SPC"
    "lt" '(lambda () (interactive) (counsel-load-theme) (zz-fix-whitespace))
    "ln" 'linum-mode
    "ta" 'align-regexp))

(use-package evil
  :demand t
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  ;; (evil-search-module 'evil-search)

  :config
  (evil-mode 1)
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

  (general-define-key :states 'motion
      zz-motion-up 'evil-previous-line
      zz-motion-down 'evil-next-line
      zz-motion-left 'evil-backward-char
      zz-motion-right 'evil-forward-char
      "<escape>" 'keyboard-quit

      ;; Windows
      (concat "C-w " zz-motion-down) 'evil-window-down
      (concat "C-w " zz-motion-up) 'evil-window-up
      (concat "C-w " zz-motion-left) 'evil-window-left
      (concat "C-w " (upcase zz-motion-up)) 'evil-window-move-very-top
      (concat "C-w " (upcase zz-motion-down)) 'evil-window-move-very-bottom
      (concat "C-w " (upcase zz-motion-left)) 'evil-window-move-far-left
      (concat "C-w " (upcase zz-motion-right)) 'evil-window-move-far-right


      ;; Frames
      "C-w x" 'other-frame
      "C-w f" 'make-frame-command

      "M-v" 'evil-paste-after)

  (general-define-key :keymaps 'normal
            "M-s" 'evil-write
            "C-s" 'evil-write
            "gV" 'zz-evil-select-pasted
            "zv" 'zz-scroll-line-to-quarter
            "RET" 'newline
            "ga" 'zz-file-stats

            ;; Buffer navigation
            "gb" 'evil-buffer
            "gr" 'revert-buffer
            "M-{" 'evil-prev-buffer
            "M-}" 'evil-next-buffer
            "C-a k" 'zz-kill-buffer
            "C-a c" 'zz-kill-other-buffers
            "C-a d" 'zz-evil-window-delete-or-die)

  (general-define-key :keymaps 'insert
            "M-s" 'zz-save-to-escape
            "C-s" 'zz-save-to-escape
            "M-v" 'evil-paste-after)

  (general-define-key :keymaps 'visual
            "M-c" 'evil-yank
            "M-v" 'evil-visual-paste)

  (general-define-key :keymaps '(minibuffer-local-map minibuffer-local-ns-map
  minibuffer-local-completion-map minibuffer-local-must-match-map
  minibuffer-local-isearch-map)
            "<escape>" 'zz-minibuffer-keyboard-quit))

(use-package minibuffer
  :straight nil
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

(use-package autorevert
  :straight nil
  :diminish auto-revert-mode)

(use-package dired
  :straight nil
  :commands (auto-revert-mode zz-dired-sort-directories-first)
  :init
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (add-hook 'dired-after-readin-hook 'zz-dired-sort-directories-first)
  :config
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)
  (general-evil-define-key 'normal dired-mode-map
    zz-motion-left nil
    zz-motion-right nil
    zz-motion-up 'dired-previous-line
    zz-motion-down 'dired-next-line
    "o" 'dired-find-alternate-file
    "RET" 'dired-find-alternate-file
    "u" 'zz-dired-up-directory
    "C-r" 'revert-buffer
    "C-p" 'zz-find-file
    "r" 'dired-do-redisplay
    "gb" 'evil-buffer
    "M-{" 'evil-prev-buffer
    "M-}" 'evil-next-buffer
    "mc" 'dired-do-copy
    "mm" 'dired-do-rename
    "ma" 'dired-create-directory
    "md" 'dired-do-delete)
  :general
  (:keymaps 'normal :prefix "SPC"
   "tt" 'zz-toggle-dired))

(use-package dired-subtree
  :after dired
  :demand t
  :config
  (general-evil-define-key 'normal dired-mode-map
    "<tab>" 'dired-subtree-toggle
    "<backtab>" 'dired-subtree-cycle))

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil)
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "ls" 'ivy-switch-buffer))
  :general
  (:states 'normal :prefix "SPC"
          "ls" 'ivy-switch-buffer)
  (:keymaps 'ivy-minibuffer-map
   [escape] 'keyboard-escape-quit
   "C-w" 'backward-kill-word
   (concat "C-" zz-motion-down) 'ivy-next-line
   (concat "C-" zz-motion-up) 'ivy-previous-line))

(use-package counsel
  :diminish counsel-mode
  :config
  (when (eq system-type 'darwin)
    (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind))
  (setq counsel-git-cmd "git ls-files --full-name --exclude-standard --others --cached --")
  (counsel-mode 1)
  (defalias 'locate #'counsel-locate)
  :general
  ("M-x" 'counsel-M-x
   "C-x C-f" 'counsel-find-file)
  (:keymaps 'normal :prefix "SPC"
   "mx" 'counsel-M-x
   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fw" 'swiper
   "kr" 'counsel-yank-pop))

(use-package projectile
  :commands (projectile-project-p)
  :diminish projectile-mode
  :config
  (setq projectile-globally-ignored-files '(".DS_Store")
        projectile-completion-system 'ivy)
  (projectile-mode)
  :general
  (:keymaps 'normal
            :prefix "SPC"
            "tr" 'zz-toggle-projectile-dired))

(use-package counsel-projectile
  :general
  ("M-P" 'counsel-projectile-switch-project
   "M-p" 'zz-find-file)
  (:states 'normal
   "C-p" 'zz-find-file)
  (:states 'normal :prefix "SPC"
   "ag" 'counsel-projectile-rg))

(use-package flyspell
  :straight nil
  :commands (flyspell-mode flyspell-buffer git-commit-turn-on-flyspell)
  :init
  (add-hook 'org-mode-hook '(lambda () (flyspell-mode) (flyspell-buffer)))
  (add-hook 'markdown-mode-hook '(lambda () (flyspell-mode) (flyspell-buffer)))
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  :config
  (flyspell-mode 1)
  (setq ispell-program-name "aspell")
  :general
  (:keymaps 'normal
            "]s" 'flyspell-goto-next-error
            "[s" 'flyspell-goto-previous-error)
  (:keymaps 'normal
            :prefix "SPC"
            "fl" 'flyspell-auto-correct-previous-word
            "sa" 'flyspell-correct-word-before-point
            "ss" '(lambda () (interactive) (ispell-change-dictionary "espanol") (flyspell-buffer))
            "se" '(lambda () (interactive) (ispell-change-dictionary "english") (flyspell-buffer))))

(use-package flyspell-correct-ivy
  :general
  (:keymaps 'normal :prefix "SPC"
            "fs" 'flyspell-correct-word-generic
            "fp" 'flyspell-correct-previous-word-generic
            "fn" 'flyspell-correct-next-word-generic))

(use-package magit
  :commands magit-read-other-branch-or-commit
  :config
  (defun zz/magit-status-with-prefix-arg ()
    "Call `magit-status` with a prefix."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'magit-status)))
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-save-repository-buffers nil
        magit-repository-directories '(("\~/workspace" . 2)))
  :general
  (:states 'normal
           :prefix "SPC"
           "gs" 'magit-status
           "gS" 'zz/magit-status-with-prefix-arg
           "go" 'zz-magit-checkout
           "gl" 'magit-log-current
           "gb" 'magit-blame
           "gm" 'magit-merge
           "gpl" 'magit-pull-branch
           "gps" 'magit-push-other
           "mgr" 'magit-ediff-resolve))

(use-package evil-magit
  :after magit
  :demand t
  :init
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  :config
  (evil-define-key evil-magit-state magit-mode-map
    zz-motion-down 'evil-next-visual-line
    zz-motion-up 'evil-previous-visual-line
    (concat "C-w " zz-motion-down) 'evil-window-down
    (concat "C-w " zz-motion-up) 'evil-window-up
    (concat "C-w " zz-motion-left) 'evil-window-left))

(use-package evil-surround
  :defer 5
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :defer 5
  :config
  (global-evil-matchit-mode 1))

(use-package evil-snipe
  :defer 3
  :diminish evil-snipe-local-mode
  :commands turn-off-evil-snipe-override-mode
  :init
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  (setq evil-snipe-spillover-scope 'whole-visible
        evil-snipe-show-prompt nil))

(use-package evil-visualstar
  :defer 3
  :config
  (global-evil-visualstar-mode))

(use-package evil-embrace
  :defer 3
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-lispy
  :diminish evil-lispy-mode
  :commands evil-lispy-mode
  :hook (emacs-lisp-mode . evil-lispy-mode))

(use-package keyfreq
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
          evil-previous-line
          evil-next-line
          evil-scroll-up
          save-buffers-kill-terminal
          save-buffers-kill-emacs))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package neotree
  :hook (neotree-mode . (lambda ()
                          (setq buffer-face-mode-face '(:family "Hack" :height 130 :width semi-condensed))
                          (buffer-face-mode)))
  :commands neotree-make-executor
  :config
  (setq neo-smart-open nil
        neo-window-width 30
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        projectile-switch-project-action 'neotree-projectile-action)
  :general
  (:states 'normal "M-1" 'neotree-toggle)
  (:keymaps 'normal
   :prefix "SPC"
   "tl" 'neotree-toggle)
  (:states 'normal
   :keymaps 'neotree-mode-map
   "o" (neotree-make-executor
        :file-fn 'neo-open-file
        :dir-fn  'neo-open-dir)
   "md" 'neotree-delete-node
   "ma" 'neotree-create-node
   "mm" 'neotree-rename-node
   "C" 'neotree-change-root
   "R" 'neotree-refresh
   "TAB" 'neotree-enter
   "?" 'describe-mode
   "A" 'neotree-stretch-toggle
   "H" 'neotree-hidden-file-toggle
   "n" 'next-line
   "p" 'previous-line
   "q" 'neotree-hide
   "u" 'neotree-select-up-node))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package flymake
  :straight nil
  :disabled t)

(use-package flycheck
  :diminish flycheck-mode
  :commands global-flycheck-mode
  :hook (prog-mode . global-flycheck-mode)
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

(use-package smartparens
  :defer 3
  :diminish smartparens-mode
  :commands smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
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

(use-package evil-smartparens
  :diminish evil-smartparens-mode
  :hook (smartparens-enabled . evil-smartparens-mode))

(use-package expand-region
  :general
  (:keymaps 'visual
            "v" 'er/expand-region
            "V" 'er/contract-region))

(use-package evil-vimish-fold
  :demand t
  :diminish evil-vimish-fold-mode
  :hook (prog-mode . evil-vimish-fold-mode)
  :general
  (:states '(normal motion) :keymaps 'evil-vimish-fold-mode-map
           (concat "z" zz-motion-up) 'evil-vimish-fold/previous-fold
           (concat "z" zz-motion-down) 'evil-vimish-fold/next-fold))

(use-package powerline
  :if window-system
  :config
  (setq powerline-image-apple-rgb t
        powerline-default-separator 'wave
        powerline-text-scale-factor 0.90))

(use-package spaceline
  :commands spaceline-spacemacs-theme
  :init
  (add-hook 'after-init-hook 'spaceline-spacemacs-theme)
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (set-face-attribute 'mode-line nil :height 140 :family "Monaco")
  (set-face-attribute 'mode-line-inactive nil :height 140 :family "Monaco")
  (spaceline-toggle-minor-modes-off))

(use-package solaire-mode
  :hook
  ((aftechange-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (setq solaire-mode-remap-modeline nil)
  (solaire-global-mode +1))

(use-package window-numbering
  :after spaceline
  :demand t
  :config
  (window-numbering-mode t)
  (setq spaceline-window-numbers-unicode t))

(use-package eyebrowse
  :after spaceline
  :demand t
  :init
  (setq eyebrowse-wrap-around t
        eyebrowse-new-workspace t
        spaceline-workspace-numbers-unicode t)
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys)
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
  :if (display-graphic-p)
  :after spaceline
  :demand t
  :config
  (nyan-mode t)
  (setq nyan-animate-nyancat t
        nyan-bar-length 22))

(use-package fancy-battery
  :after spaceline
  :demand t
  :config
  (fancy-battery-mode)
  (setq fancy-battery-show-percentage t))

(use-package git-gutter
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
  :after evil
  :demand t
  :config
  (setq anzu-cons-mode-line-p t)
  (global-anzu-mode +1))

(use-package font-lock+)

(use-package all-the-icons
  :config
  (require 'font-lock+))

(use-package all-the-icons-dired
  :commands all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package disable-mouse
  :defer 5
  :diminish disable-mouse-global-mode
  :config
  (global-disable-mouse-mode))

(use-package evil-multiedit
  :demand t
  :config
  ;; TODO: Move this bindings to general (note these are custom states)
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  (define-key evil-multiedit-state-map (kbd "o") 'evil-multiedit-toggle-or-restrict-region)
  (define-key evil-motion-state-map (kbd "o") 'evil-multiedit-toggle-or-restrict-region)
  (define-key evil-multiedit-state-map (kbd "]c") 'evil-multiedit-next)
  (define-key evil-multiedit-state-map (kbd "[c") 'evil-multiedit-prev)
  (define-key evil-multiedit-insert-state-map (kbd "]c") 'evil-multiedit-next)
  (define-key evil-multiedit-insert-state-map (kbd "[c") 'evil-multiedit-prev)
  :general
  (:keymaps 'normal
   "M-d" 'evil-multiedit-match-and-next
   "M-D" 'evil-multiedit-match-and-prev)
  (:keymaps 'visual
   "M-d" 'evil-multiedit-match-and-next
   "M-D" 'evil-multiedit-match-and-prev
   "R" 'evil-multiedit-match-all))

(use-package whitespace
  :diminish global-whitespace-mode
  :hook ((web-mode . (lambda () (setq-local whitespace-line-column 101)))
         (prog-mode . (lambda () (setq-local whitespace-line-column 80)))
         (java-mode . (lambda () (setq-local whitespace-line-column 100)))
         (python-mode . (lambda () (setq-local whitespace-line-column 79))))
  :init (global-whitespace-mode 1)
  :config
  ;; TODO This function does not work well on light themes
  (defun zz-fix-whitespace ()
    (interactive)
    "Fix whitespace-tab face (useful after changing themes)."
    (set-face-attribute 'whitespace-tab nil :background nil :foreground "gridColor"))
  (setq whitespace-style '(face trailing tabs tab-mark lines-tail))
  ;; Use M-x list-colors-display to see names to color references
  (set-face-attribute 'whitespace-tab nil :background nil :foreground "gridColor"))

(use-package simple
  :straight nil
  :diminish auto-fill-function
  :hook ((java-mode . (lambda () (set-fill-column 110)))
         (python-mode . (lambda () (set-fill-column 79)))
         ((markdown-mode org-mode prog-mode) . (lambda ()
                                                 (turn-on-auto-fill)
                                                 (set-fill-column 80))))
  :general
  (:states 'normal :keymaps 'messages-buffer-mode-map
           "q" 'evil-buffer
           "<escape>" 'evil-buffer))

(use-package emmet-mode
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

(use-package drag-stuff
  :diminish drag-stuff-mode
  :init
  (drag-stuff-global-mode 1)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys)
  :general
  (:keymaps '(normal visual)
   (concat "M-" zz-motion-up) 'drag-stuff-up
   (concat "M-" zz-motion-down) 'drag-stuff-down
   (concat "M-" zz-motion-left) 'drag-stuff-left
   (concat "M-" zz-motion-right) 'drag-stuff-right))

(use-package help-mode
  :straight nil
  :init
  (add-hook 'help-mode-hook
            '(lambda ()
               (evil-define-key 'motion help-mode-map (kbd "gb") 'evil-buffer)
               (evil-define-key 'motion help-mode-map (kbd zz-motion-left) 'evil-backward-char)
               (evil-define-key 'motion help-mode-map (kbd zz-motion-right) 'evil-forward-char)
               (evil-define-key 'motion help-mode-map (kbd zz-motion-down) 'evil-next-line)
               (evil-define-key 'motion help-mode-map (kbd zz-motion-up) 'evil-previous-line))))

(use-package eshell
  :straight nil
  :init
  (add-hook 'eshell-mode-hook
            '(lambda ()
               (add-hook 'evil-insert-state-entry-hook 'zz-shell-insert nil t)
               (general-evil-define-key 'insert eshell-mode-map
                 "C-l" 'zz-eshell-clear-buffer
                 "C-p" 'eshell-previous-matching-input-from-input
                 "C-n" 'eshell-previous-matching-input-from-input)))
  :config
  (setq eshell-cmpl-ignore-case t)
  ;; Error (use-package): eshell :config: Symbol’s value as variable is void: eshell-output-filter-functions
  ;; (delete `eshell-postoutput-scroll-to-bottom eshell-output-filter-functions)
  :general
  (:keymaps 'normal :prefix "SPC"
            "sh" 'zz-eshell-current-dir))

(use-package term
  :straight nil
  :init
  (add-hook 'term-mode-hook
            '(lambda () (add-hook 'evil-insert-state-entry-hook 'zz-shell-insert nil t)))
  :general
  (:keymaps 'normal :prefix "SPC"
            "zsh" 'zz-zshell-current-dir))

(use-package undo-tree
  :straight nil
  :diminish undo-tree-mode)

(use-package htmlize)
(use-package esup)

(use-package frame
  :straight nil
  ;; :hook (after-init . toggle-frame-fullscreen)
  :init
  ;; no-title-bars was replaced with following setting
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;; natural-title-bar replaced with following setting dark or ligth
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq frame-title-format '("%b"))
  ;; When no-title-bar allows to fill the whole window (with menubar)
  (setq frame-resize-pixelwise t)
  ;; For others solutions: https://stackoverflow.com/a/20411530/2948807
  ;; Avoid new workspace on fullscreen
  (setq ns-use-native-fullscreen nil)
  :config
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "full[screen]" 'toggle-frame-fullscreen))
  :general
  ("M-RET" 'toggle-frame-maximized))

(use-package hide-mode-line
  :commands hide-mode-line-mode
  :hook ((completion-list-mode neotree-mode) . hide-mode-line-mode)
  :config
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "mode[line]" 'hide-mode-line-mode)))

(use-package centered-window
  :commands centered-window-mode
  :config
  (setq cwm-centered-window-width 80)
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "free[mode]" 'centered-window-mode))
  :general
  (:keymaps 'normal :prefix "SPC"
   "cw" 'centered-window-mode))

;; ===================================================
;; LANGUAGE SPECIFIC MODES AND PACKAGES
;; ===================================================

(use-package cask-mode)
(use-package json-mode)
(use-package dockerfile-mode)
(use-package php-mode)

(use-package phpunit
  :after php-mode
  :general
  (:keymaps 'normal :prefix "SPC"
   "rr" 'phpunit-current-test
   "rt" 'phpunit-current-class
   "ra" 'phpunit-current-project))

(use-package web-mode
  :mode ("\\.blade\\.php\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'" "\\.css\\'")
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-engines-alist '(("django" . ".*/python/django/.*\\.html\\'"))))

(use-package rbenv
  :commands global-rbenv-mode
  :init
  (add-hook 'ruby-mode-hook 'global-rbenv-mode)
  :config
  (setq rbenv-show-active-ruby-in-modeline nil))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (setq-default js-indent-level 2
                js-chain-indent t
                js2-basic-offset 2
                js2-strict-missing-semi-warning nil)
  (js2-imenu-extras-mode))

(use-package tern
  :if (locate-file "tern" exec-path)
  :hook ((js2-mode . tern-mode)
         (rjsx-mode . tern-mode))
  :config
  (add-to-list 'load-path "~/.config/yarn/global/node_modules/tern/emacs/tern.el")
  (autoload 'tern-mode "tern.el" nil t))

(use-package rjsx-mode
  :mode (("\\.jsx$" . rjsx-mode)
         ("components/.+\\.js$" . rjsx-mode)))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(use-package eslintd-fix
  :hook ((js2-mode . eslintd-fix-mode)
         (rjsx-mode . eslintd-fix-mode)
         (js2-mode . zz-eslint-local-node-modules)
         (rjsx-mode . zz-eslint-local-node-modules))
  :config
  (defun zz-eslint-local-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/.bin/eslint"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint)))))

(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :config
  (setq org-plantuml-jar-path
        (concat (straight--repos-dir "plantuml-mode/bin") "plantuml.jar")))

(use-package feature-mode
  :mode "\\.feature\\'")

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package apib-mode
  :mode "\\.apib\\'")

(use-package nginx-mode)

(use-package fountain-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  :config
  (setq markdown-gfm-use-electric-backquote nil)
  :general
  (:keymaps 'markdown-mode-map :states 'normal
   "TAB" 'markdown-cycle))

(use-package tex-mode
  :straight nil
  :hook (tex-mode . (lambda ()
                      (setq indent-tabs-mode nil
                            tab-stop-list (number-sequence 2 200 2)
                            tab-width 2
                            evil-shift-width 2))))

(use-package octave
  :straight nil
  :mode ("\\.m$" . octave-mode))

(use-package lisp-mode
  :straight nil
  :hook (emacs-lisp-mode . (lambda ()
                             (setq-local lisp-indent-function #'zz-lisp-indent-function))))

(use-package haskell-mode
  :hook (haskell-mode . (lambda () (evil-smartparens-mode -1)))
  :config
  (let ((stack-command "stack build --pedantic --fast"))
    (setq haskell-stylish-on-save t
          haskell-compile-cabal-build-command (concat "cd %s && " stack-command)
          projectile-project-compilation-cmd stack-command
          projectile-project-test-cmd (concat stack-command " --test")
          flycheck-ghc-language-extensions '("OverloadedStrings")))
  :general
  (:keymaps 'haskell-mode-map :states 'normal :prefix "SPC"
   "rr" 'haskell-compile
   "ra" 'projectile-compile-project
   "rt" 'projectile-test-project))

(use-package purescript-mode
  :hook (purescript-mode . turn-on-purescript-indentation)
  :diminish purescript-indentation-mode)

(use-package elm-mode
  :init
  (add-hook 'elm-mode-hook
            '(lambda ()
               (set (make-local-variable 'company-backends) '(company-elm))
               (company-mode t)
               (add-hook 'before-save-hook
                         'elm-mode-format-buffer
                         nil
                         'local))))

(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :hook (scala-mode . (lambda () (evil-smartparens-mode -1))))

(use-package solidity-mode)

(use-package nix-mode
  :mode ("\\.nix$" . nix-mode))

(use-package go-mode
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook 'gofmt-before-save nil 'local)))
  :config
  (setq gofmt-command "goimports")
  :general
  (:keymaps 'go-mode-map :states 'normal
   "M-RET" 'godef-jump))

(use-package go-eldoc
  :diminish eldoc-mode
  :commands go-eldoc-setup
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package company
  :defer 5
  :diminish company-mode
  :config
  (global-company-mode)
  (company-tng-configure-default)
  (setq company-dabbrev-downcase 0
        company-idle-delay 0.2))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package company-web
  :commands company-mode
  :init
  (add-hook 'web-mode-hook (lambda ()
                             (set (make-local-variable 'company-backends) '(company-web-html))
                             (company-mode t))))

(use-package company-tern
  :commands company-mode
  :hook (tern-mode . (lambda ()
                       (set (make-local-variable 'company-backends) '(company-tern))
                       (company-mode t)))
  :config
  (setq company-tern-property-marker nil
        company-tooltip-align-annotations t))

(use-package company-go
  :commands company-mode
  :init
  (add-hook 'go-mode-hook '(lambda ()
                             (set (make-local-variable 'company-backends) '(company-go))
                             (company-mode t))))

(use-package pyvenv
  :config
  (setq pyvenv-virtualenvwrapper-python
        (or (getenv "VIRTUALENVWRAPPER_PYTHON")
            (executable-find "python3"))))

(use-package elpy
  :commands (flycheck-mode elpy-format-code)
  :hook ((elpy-mode . flycheck-mode) ;; TODO global-flycheck-mode does this?
         (python-mode . (lambda ()
                          (add-hook 'before-save-hook 'elpy-format-code
                                    nil
                                    'local))))
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-python-command "python3"
        elpy-modules (delq 'elpy-module-flymake elpy-modules)
        python-shell-interpreter "python3"
        python-shell-interpreter-args "-i --simple-prompt"
        flycheck-python-flake8-executable (executable-find "flake8"))
  (delete `elpy-module-highlight-indentation elpy-modules))

;; ===================================================
;; VANILLA PACKAGES
;; ===================================================
(use-package bookmark
  :straight nil
  :config
  (define-key bookmark-bmenu-mode-map (kbd (concat "C-" zz-motion-up)) 'previous-line)
  (define-key bookmark-bmenu-mode-map (kbd (concat "C-" zz-motion-down)) 'next-line)
  :general
  (:keymaps 'normal :prefix "SPC"
   "bl" 'bookmark-bmenu-list
   "bk" 'counsel-bookmark
   "bm" 'bookmark-set))

;; ===================================================
;; ORG RELATED
;; ===================================================

(use-package org
  :init
  (setq org-emphasis-regexp-components '("-[:space:][:alpha:]('\"{"
                                         "-[:space:][:alpha:].,:!?;'\")}\\["
                                         "[:space:]"
                                         "."
                                         4))
  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  :config
  (require 'org-tempo)
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t)
                                 (python . t)
                                 (ruby . t)
                                 (awk . t)
                                 (emacs-lisp . t)
                                 (haskell . t)))
  (setq org-log-done t
        org-ellipsis " ⤵"
        org-src-fontify-natively t
        org-todo-keywords '((sequence "TODO" "IN-PROGRESS"
                                      "|" "DONE" "DELEGATED" "CANCELED"))
        org-agenda-files (list "~/Documents/org/free.org"
                               "~/Documents/org/paid.org"
                               "~/Documents/org/todo.org"))
  :general
  (:keymaps 'normal :prefix "SPC"
            "oa" 'org-agenda)
  (:keymaps 'org-mode-map :states 'normal :prefix "SPC"
            "ot" 'org-todo
            "op" 'org-set-property
            "os" 'org-schedule
            "ox" 'org-export-dispatch
            "j" 'evil-join)
  (:keymaps 'org-mode-map :states '(insert normal)
            (concat "M-" zz-motion-up) 'org-metaup
            (concat "M-" zz-motion-down) 'org-metadown
            (concat "M-" zz-motion-left) 'org-metaleft
            (concat "M-" zz-motion-right) 'org-metaright
            (concat "M-" (upcase zz-motion-up)) 'org-shiftmetaup
            (concat "M-" (upcase zz-motion-down)) 'org-shiftmetadown
            (concat "M-" (upcase zz-motion-right)) 'org-shiftmetaright
            (concat "M-" (upcase zz-motion-left)) 'org-shiftmetaleft)
  (:keymaps 'org-mode-map :states 'normal
            (upcase zz-motion-up) 'org-shiftup
            (upcase zz-motion-down) 'org-shiftdown
            (upcase zz-motion-right) 'org-shiftright
            (upcase zz-motion-left) 'org-shiftleft)
  (:keymaps 'org-agenda-mode-map :states '(insert emacs)
            (concat "C-" zz-motion-up) 'org-agenda-previous-line
            (concat "C-" zz-motion-down) 'org-agenda-next-line))

(use-package org-crypt
  :after org
  :demand t
  :straight nil
  :config
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key "zzantares@gmail.com"))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-ref
  :after org
  :defer 3
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite
        reftex-default-bibliography '("~/workspace/bibliography/references.bib")
        org-ref-bibliography-notes "~/workspace/bibliography/notes.org"
        org-ref-default-bibliography '("~/workspace/bibliography/references.bib")
        org-ref-pdf-directory "~/workspace/bibliography/bibtex-pdfs/")
  :general
  (:keymaps 'normal
            :prefix "SPC"
            "oc" 'org-ref-ivy-insert-cite-link
            "or" 'org-ref-ivy-insert-ref-link
            "ol" 'org-ref-ivy-insert-label-link)
  (:keymaps 'org-mode-map
            :states '(insert emacs)
            "C-c C-]" 'org-ref-insert-cite-with-completion
            "C-c ]" 'org-ref-insert-cite-with-completion))

(use-package ox-gfm
  :after ox
  :demand t)

(use-package ox-hugo
  :after ox
  :demand t)

(use-package ox-latex
  :after ox
  :demand t
  :straight nil
  :config
  (setq org-latex-caption-above nil)
  (setq org-latex-pdf-process '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                                "bibtex $(basename %b)"
                                "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                                "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))
  (add-to-list 'org-latex-classes
               `("llncs"
                 "\\documentclass[runningheads]{llncs}
[DEFAULT-PACKAGES]
[PACKAGES]
\\renewcommand\\UrlFont{\\color{blue}\\rmfamily}
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" "\\newpage" "\\subsection*{%s}" "\\newpage")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; ox-reveal makes org easy templates not work
(use-package ox-reveal
  :after ox
  :demand t
  :config
  (->> org-structure-template-alist
     (cdr) ;; the last value is added by ox-reveal and is set up the old way
     (cons `(,org-reveal-note-key-char . "notes")) ;; the new way
     (setq org-structure-template-alist))
  ;; Black full screen on chrome https://cdn.jsdelivr.net/npm/reveal.js@3.3.0
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.6.0"
        org-reveal-mathjax t))


;; ===================================================
;; THEMES
;; ===================================================
(use-package seti-theme :no-require t)
(use-package eziam-theme :no-require t)
(use-package planet-theme :no-require t)
(use-package flatui-theme :no-require t)
(use-package molokai-theme :no-require t)
(use-package kaolin-themes :no-require t)
(use-package gruvbox-theme :no-require t)
(use-package dracula-theme :no-require t)
(use-package base16-theme :no-require t)
(use-package oceanic-theme :no-require t)
(use-package material-theme :no-require t)
(use-package twilight-theme :no-require t)
(use-package sublime-themes :no-require t)
(use-package intellij-theme :no-require t)
(use-package farmhouse-theme :no-require t)
(use-package solarized-theme :no-require t)
(use-package white-sand-theme :no-require t)
(use-package soft-stone-theme :no-require t)
(use-package hc-zenburn-theme :no-require t)
(use-package danneskjold-theme :no-require t)
(use-package anti-zenburn-theme :no-require t)
(use-package dakrone-light-theme :no-require t)
(use-package apropospriate-theme :no-require t)
(use-package green-phosphor-theme :no-require t)
(use-package green-phosphor-theme :no-require t)
(use-package twilight-bright-theme :no-require t)
(use-package twilight-anti-bright-theme :no-require t)
(use-package color-theme-sanityinc-tomorrow :no-require t)
(use-package chocolate-theme
  :straight (chocolate-theme :type git :host github :repo "SavchenkoValeriy/emacs-chocolate-theme")
  :no-require t)
(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
  (load-theme 'doom-nord)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; ===================================================
;; GENERAL SETTINGS
;; ===================================================

(setq user-full-name "Julio César"
      user-mail-address "zzantares@gmail.com")

;; Use proper ls in emacs mac os
(setq insert-directory-program "/usr/local/bin/gls")

;; Can answer 'y' instead of 'yes' when emacs asks a question
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable sound on bell
(setq ring-bell-function 'ignore)

;;  Disable blink cursor
(blink-cursor-mode -1)

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
(setq-default line-spacing 5
              ;; line-spacing 15
              indent-tabs-mode nil
              tab-width 4)

;; Window border separator
(set-face-attribute 'vertical-border nil :foreground "black")

;; Font settings
;; (set-face-attribute 'default nil :height 180 :family "Inconsolata")
;; (set-face-attribute 'default nil :height 200 :family "Consolas")
;; (set-face-attribute 'default nil :height 170 :family "Ubuntu Mono")
;; (set-face-attribute 'default nil :height 160 :family "Operator Mono")
;; (set-face-attribute 'default nil :height 150 :family "Fira Code")
;; (set-face-attribute 'default nil :height 150 :family "Hack")
;; (set-face-attribute 'default nil :height 150 :family "Monaco")
(set-face-attribute 'default nil :height 180 :family "Menlo")
;; (set-face-attribute 'default nil :height 160 :family "Roboto Mono")
;; (set-face-attribute 'default nil :height 170 :family "Fantasque Sans Mono")
;; (set-face-attribute 'default nil :height 160 :family "Fira Mono")

;; Only applies to Yamamoto Mitsuharu's patch
(when (boundp 'mac-carbon-version-string)
  (mac-toggle-tab-bar)
  (when (display-graphic-p)
    ;; Use Fira Code ligatures when this font is activated
    (let ((font-query (query-font (face-attribute 'default :font))))
      (when (string-match-p (regexp-quote "Fira Code") (elt font-query 0))
        (mac-auto-operator-composition-mode)))))
