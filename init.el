;; init.el --- My emacs init.el file!
;;; Commentary:
;; Things to try:
;;; https://www.reddit.com/r/emacs/comments/51jvai/making_modern_emacs_themes/

;; Enable "Go to definition" in Emacs

;; Configure info on Ctrl + =, to appear in :norm ga

;; Flake8 and pyenv issue solved? https://emacs.stackexchange.com/a/32421/12340

;; Marks are not persisted between sessions
;; Reported: https://bitbucket.org/lyro/evil/issues/674/marks-are-not-persisted-between-sessions

;; Show popup of line with keyboard shurtcut (para ver la información tipo IDE)

;; Algunas veces cuando escapo con "kj" la identación del simbolo en esa linea cambia
;; Reportado en: https://github.com/syl20bnr/evil-escape/issues/69

;; Vim folds parece que deshacen el colorscheme?

;; Evil cursor shape http://emacs.stackexchange.com/questions/7403/evil-cursor-change-by-state

;;; Code:
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; Activate installed packages
(setq package-enable-at-startup nil)
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Check all packages in `PACKAGES' is installed, ask to install if they're not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (message (format "Package %s is missing.  Installing... " package))
       (package-install package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Is possible to use Cask (https://github.com/cask/cask) for this
(ensure-package-installed 'evil
                          'magit
                          'evil-surround
                          'evil-escape
                          'evil-leader
                          'evil-magit
                          'evil-matchit
                          'evil-snipe
                          'evil-visualstar
                          'evil-embrace
                          'evil-lispy
                          'drag-stuff
                          'keyfreq
                          'flyspell-lazy
                          'exec-path-from-shell
                          'projectile
                          'cask-mode
                          'helm
                          'helm-projectile
                          'helm-flyspell
                          'neotree
                          'buffer-move
                          'centered-window-mode
                          'diminish
                          'yasnippet
                          'flycheck
                          'smartparens
                          'expand-region
                          'web-mode
                          'php-mode
                          'phpunit
                          'rbenv
                          'js2-mode
                          'json-mode
                          'feature-mode
                          'company
                          'company-web
                          'company-tern
                          'company-go
                          'vimish-fold
                          'evil-vimish-fold
                          'dired-subtree
                          'clojure-mode
                          'clojure-mode-extra-font-locking
                          'cider
                          'alchemist
                          'elm-mode
                          'org
                          'org-bullets
                          'ox-gfm
                          'ox-reveal
                          'spaceline
                          'window-numbering
                          'eyebrowse
                          'nyan-mode
                          'fancy-battery
                          'git-gutter
                          'evil-anzu
                          'all-the-icons
                          'apib-mode
                          'go-mode
                          'go-eldoc
                          'scala-mode
                          'sbt-mode
                          'markdown-mode
                          'dockerfile-mode
                          'highlight-chars
                          'base16-theme
                          'apropospriate-theme
                          'oceanic-theme
                          'flatui-theme
                          'twilight-anti-bright-theme
                          'twilight-bright-theme
                          'twilight-theme
                          'molokai-theme
                          'solarized-theme
                          'dracula-theme
                          'seti-theme
                          'disable-mouse
                          'gruvbox-theme
                          'material-theme)

;; use the same $PATH as in shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Custom code
(add-to-list 'load-path "~/.emacs.d/libs")
(require 'company-simple-complete)
;; (require 'aurora-theme)
;; (require 'lawrence-theme)  // Only enable to use lawrence

;; Functions ==================================================================

;;; Kill other buffers
(defun zz-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (message ":BufOnly"))

;;; Toggles projectile-dired buffer
(defun zz-toggle-projectile-dired ()
  "Toggle projectile-dired buffer."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (evil-delete-buffer (current-buffer))
    (if (projectile-project-p)
        (projectile-dired)
      (dired (file-name-directory buffer-file-name)))))

(defun zz-toggle-dired ()
  "Toggle dired buffer."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (evil-delete-buffer (current-buffer))
    (dired (file-name-directory (or buffer-file-name "~/")))))

(defun zz-eshell-exit ()
  "Terminates the process in the eshell buffer then deletes it thus exiting."
  (interactive)
  (eshell-quit-process)
  (evil-delete-buffer (current-buffer)))

(defun zz-on-asus ()
  "Return non-nil if we are on asus monitor."
  (and (= 2048 (display-pixel-width))
       (= 1152 (display-pixel-height))))

(defun zz-on-pro ()
  "Return non-nil if we are on macbook pro display."
  (and (= 1280 (display-pixel-width))
       (= 800 (display-pixel-height))))

(defun zz-save-to-escape ()
  "First save the buffer and then escape from insert."
  (interactive)
  (save-buffer)
  (evil-escape))

(defun zz-magit-branch-and-checkout (branch start-point &optional args)
  "Create and checkout BRANCH at revision START-POINT updating the mode-line.
\n(git checkout [ARGS] -b BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create and checkout branch"))
  (if (string-match-p "^stash@{[0-9]+}$" start-point)
      (magit-run-git "stash" "branch" branch start-point)
    (magit-call-git "checkout" args "-b" branch start-point)
    (magit-branch-maybe-adjust-upstream branch start-point)
    (magit-refresh)
    (revert-buffer t t)))

;; Magit checkout with mode line updated
(defun zz-magit-checkout (revision)
  "Check out a REVISION branch updating the mode line (reverts the buffer)."
  (interactive (list (magit-read-other-branch-or-commit "Checkout")))
  (magit-checkout revision)
  (revert-buffer t t))

;; Eshell helper functions
(defun zz-eshell-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun zz-eshell-insert ()
  "When entering insert mode do it at the prompt line."
  (interactive)
  (evil-goto-line)
  (evil-append-line nil))

(defun zz-zshell-insert ()
  "When entering insert mode do it at the prompt line."
  (interactive)
  (evil-goto-line)
  (evil-previous-visual-line)
  (evil-backward-char)
  (evil-append-line nil))

(defun zz-find-file ()
  "When in a project use projectile to find a file otherwise use helm."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (helm-for-files)))

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

(defun zz-evil-window-delete-or-die ()
  "Delete the selected window.  If this is the last one, exit Emacs."
  (interactive)
  (condition-case nil
      (evil-window-delete)
    (error (condition-case nil
               (delete-frame)
             (error (evil-quit))))))

(defun zz-git-gutter-stage-hunk ()
  "Don't ask for confirmation when staging a hunk."
  (interactive)
  (setq git-gutter:ask-p nil)
  (git-gutter:stage-hunk)
  (setq git-gutter:ask-p t))

(defun zz-kill-buffer ()
  "Alias for killing current buffer."
  (interactive)
  (kill-buffer nil))

(defun zz-scroll-half-page (direction)
  "Scroll half page up if DIRECTION is non-nil,other wise will scroll half page down."
  (let ((opos (cdr (nth 6 (posn-at-point)))))
    ;; opos = original position line relative to window
    (move-to-window-line nil)  ;; Move cursor to middle line
    (if direction
        (recenter-top-bottom -1)  ;; Current line becomes last
      (recenter-top-bottom 0))  ;; Current line becomes first
    (move-to-window-line opos)))

(defun zz-scroll-half-page-down ()
  "Scrolls exactly half page down keeping cursor position."
  (interactive)
  (zz-scroll-half-page nil))

(defun zz-scroll-half-page-up ()
  "Scrolls exactly half page up keeping cursor position."
  (interactive)
  (zz-scroll-half-page t))

(defun zz-scroll-line-to-quarter ()
  "Scrolls windows so that the current line ends at ~25% of the window."
  (interactive)
  (evil-scroll-line-to-top (- (line-number-at-pos) 5))
  (evil-next-visual-line 5))

(defun zz-dired-up-directory ()
  "Go up one level in the directory structure reusing dired buffer."
  (interactive)
  (find-alternate-file ".."))

(defun zz-magit-status-p ()
  "Gives a non-nill value if `major-mode' is magit-status-mode."
  (or (derived-mode-p 'magit-status-mode)
      (derived-mode-p 'magit-log-mode)
      (derived-mode-p 'magit-revision-mode)))

(defun zz-compilation-mode-p ()
  "Gives non-nil if `major-mode' is `compilation-mode' or derived."
  (derived-mode-p 'compilation-mode))

(defun zz-help-mode-p ()
  "Gives non-nil if `major-mode' is `help-mode' or derived."
  (derived-mode-p 'help-mode))

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

(defun zz-evil-select-pasted ()
  "Visually select last paste text."
  (interactive)
  (evil-goto-mark ?[)
  (evil-visual-char)
  (evil-goto-mark ?]))

(defun zz-evil-insert-state (&optional argpos)
  "Enter evil-insert-state only if buffer is not read only.
If ARGPOS is not specified insert will happen at point."
  (interactive)
  (if buffer-read-only
      (message (concat "Buffer is read-only: " (buffer-name)))
    (cond ((string= "I" argpos)
           (progn (evil-next-line-1-first-non-blank)
                 (evil-insert-state)))
          ((string= "A" argpos) (evil-append-line 0))
          ((string= "a" argpos) (evil-append 0))
          (t (evil-insert-state)))))

(defun zz-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; move point to previous error
;; based on code by hatschipuh at
;; http://emacs.stackexchange.com/a/14912/2017
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

;; Interpret ANSI color codes
(require 'ansi-color)

(defun zz-colorize-buffer ()
  "When on shell or terminal buffers interpret color ansi codes."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'zz-colorize-buffer)

;; ============================================================================

;; Helm
(require 'helm)
(require 'helm-config)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(setq helm-for-files-preferred-list (list helm-source-files-in-current-dir
                                          helm-source-recentf
                                          helm-source-buffers-list))

;; Helm M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;; ============================================================================
;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
;; Safe to delete if no answer to
(helm-projectile-on)
;; (setq projectile-switch-project-action 'helm-projectile-find-file)

;; ============================================================================
;; Company
(add-hook 'after-init-hook '(lambda ()
                              (interactive)
                              (global-company-mode)
                              (setq company-dabbrev-downcase 0)
                              (setq company-idle-delay 0.2)))

;; ============================================================================
;; Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . ".*/adminpanel\\.hosting/.*\\.html\\'")
        ("django" . ".*/python/django/.*\\.html\\'")))

;;; Enable company-web only on web-mode
(add-hook 'web-mode-hook (lambda ()
                           (setq evil-shift-width 2)
                           (setq web-mode-css-indent-offset 2)
                           (setq web-mode-markup-indent-offset 2)
                           (setq web-mode-code-indent-offset 2)
                           (set (make-local-variable 'company-backends) '(company-web-html))
                           (company-mode t)))

;; JavaScript mode js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;;; Enable company-tern only on js2-mode
(add-hook 'js2-mode-hook '(lambda ()
                            (tern-mode t)
                            (set (make-local-variable 'company-backends) '(company-tern))
                            (company-mode t)))

;; Syntax highlighting in .feature files
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;; (add-hook 'feature-mode '(lambda ()
;; ;;                            (setq feature-default-language "en")))

;; ============================================================================
;; Use all-the-icons on dired mode
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; show new files instantly

;; ============================================================================
;; Emmet mode
(setq emmet-preview-default nil)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'nxml-mode-hook 'emmet-mode)

;; ============================================================================
;; Magit
;; Show magit buffer in the whole window
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(setq magit-save-repository-buffers nil)  ;; Don't ask "Save file?" when in magit

;; ============================================================================
;; Neotree
;; Use a specific witdth for the tree
(setq neo-window-width 30)

;; ============================================================================
;; Keyfreq (keyfreq-show to display report)
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
(keyfreq-autosave-mode 1)

;; ============================================================================

;; Battery status on the mode-line
(add-hook 'after-init-hook #'fancy-battery-mode)
(setq fancy-battery-show-percentage t)

;; Anzu: current match and total matches on search
(with-eval-after-load 'evil
  (require 'evil-anzu)
  (setq anzu-cons-mode-line-p nil))

;; Disable mouse
(global-disable-mouse-mode)

;; Flycheck (like syntastic)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-idle-change-delay 5)
(setq flycheck-check-syntax-automatically '(save idle-change))

;; Flycheck errors only in gui
(setq flycheck-indication-mode nil)

;; Ensure newline char at EOF on prog-mode
(add-hook 'prog-mode-hook '(lambda ()
                             (interactive)
                             (setq require-final-newline t)))

;; Window-numbering
(window-numbering-mode t)
(setq spaceline-window-numbers-unicode t)

;; Nyan cat variables
(setq nyan-animate-nyancat t)
(setq nyan-bar-length 22)

;; Vimish Fold
(vimish-fold-global-mode 1)
(evil-vimish-fold-mode 1)

;; Search regions ala Vim
(global-evil-visualstar-mode)

;; Enable evil-embrace (complements evil-surround)
(evil-embrace-enable-evil-surround-integration)

;; Smartparens: Autoclose parenthesis and more
(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)

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
(sp-pair "[" nil :post-handlers '((zz-newline-and-enter-sexp "RET")))

;; Drag stuff
(require 'drag-stuff)
(add-to-list 'drag-stuff-except-modes 'org-mode)
(drag-stuff-global-mode t)

;; Location for theme changer mainly
(setq user-full-name "Julio César")
(setq user-mail-address "zzantares@gmail.com")
(setq calendar-location-name "Zacatecas, México")
(setq calendar-latitude 22.77)
(setq calendar-longitude -102.57)

;; Yasnippets
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Org mode
;; (require 'org)
(require 'org-indent)
(org-indent-mode -1)
(setq org-log-done t)
(setq org-ellipsis " ⤵")
(setq org-src-fontify-natively t)
(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(setq org-agenda-files (list "~/Documents/org/free.org"
                             "~/Documents/org/paid.org"
                             "~/Documents/org/todo.org"))

(setq solarized-use-variable-pitch nil)

;; Org-reveal
(require 'ox-reveal)
(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

;; Org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda ()
                           (flyspell-mode)
                           (flyspell-buffer)
                           (org-bullets-mode 1)
                           (turn-on-auto-fill)
                           (set-fill-column 80)))

;; ============================================================================
;; Options
;;; Backups in backup dir
(setq backup-by-copying t   ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)   ; use versioned backups

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs-saves/" t)))

;; Can answer 'y' instead of 'yes' when emacs asks a question
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable sound on bell
(setq ring-bell-function 'ignore)

;;  Disable blink cursor
(blink-cursor-mode -1)

;; Delete trailing spaces after saving
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Don't garbage collect too often
(add-hook 'minibuffer-setup-hook #'(lambda () (interactive) (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda () (interactive) (setq gc-cons-threshold 800000)))

(add-hook 'neotree-mode-hook '(lambda ()
                                (interactive)
                                (setq buffer-face-mode-face '(:family "Hack" :height 130 :width semi-condensed))
                                (buffer-face-mode)))
;; Font size
;; (set-face-attribute 'default nil :height 150 :family "Hack")
;; (set-face-attribute 'default nil :height 150 :family "Monaco")
;; (set-face-attribute 'default nil :height 150 :family "Menlo")
;; (set-face-attribute 'default nil :height 160 :family "Roboto Mono")
(set-face-attribute 'default nil :height 180 :family "Inconsolata")
;; (set-face-attribute 'default nil :height 170 :family "Fantasque Sans Mono")
;; (set-face-attribute 'default nil :height 160 :family "Fira Mono")

;; When having two direds use each other path as targets for commands
(setq dired-dwim-target t)
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

(add-hook 'dired-after-readin-hook 'zz-dired-sort-directories-first)

(add-hook 'elixir-mode-hook '(lambda ()
                               (setq evil-shift-width 2)
                               (alchemist-mode 1)))

(add-hook 'elm-mode-hook '(lambda ()
                       (set (make-local-variable 'company-backends) '(company-elm))
                       (company-mode t)
                       (setq elm-format-on-save t)
                       (setq elm-sort-imports-on-save t)
                       (setq elm-format-elm-version "0.18")))

;; Use spaces instead of tabs with propper tabstops
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

;; go-mode
(add-hook 'go-mode-hook '(lambda ()
                           (add-hook 'before-save-hook
                                     '(lambda ()
                                        (gofmt-before-save)
                                        (go-remove-unused-imports))
                                     nil
                                     'local)
                           (go-eldoc-setup)
                           (set (make-local-variable 'company-backends)
                                '(company-go))
                           (company-mode t)))

;; ruby-mode
(require 'smartparens-ruby)
(setq ruby-indent-level 2)  ;; Indent by 2
(setq rbenv-show-active-ruby-in-modeline nil)  ;; Don't show ruby version

;; Treat gemfile and other files as ruby files
(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(add-hook 'ruby-mode-hook '(lambda ()
                             (global-rbenv-mode)
                             (setq evil-shift-width 2)
                             (setq ruby-insert-encoding-magic-comment nil)))

(add-hook 'csharp-mode-hook '(lambda ()
                               (global-auto-revert-mode t)))

;;; Remove all gui toolbars
(menu-bar-mode -1)
;; (toggle-scroll-bar -1) Disables scrollbars
(set-scroll-bar-mode nil)
(tool-bar-mode -1)

;; Line spacing
(setq-default line-spacing 14)

;; Spacemacs like powerline
;; Valid Values: alternate, arrow, arrow-fade, bar, box, brace,
;; butt, chamfer, contour, curve, rounded, roundstub, wave, zigzag,
;; (setq powerline-default-separator 'box)
(setq powerline-default-separator 'wave)
;; (setq powerline-default-separator 'slant)
;; (setq powerline-default-separator 'arrow)

(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-helm-mode)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(spaceline-toggle-minor-modes-off)

;;; Theming
;; (load-theme 'base16-default-dark t)
;; (load-theme 'base16-ocean-dark t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)
;; (load-theme 'material t)
;; (load-theme 'oceanic t)
;; (load-theme 'molokai t)
;; (load-theme 'whiteboard t)
;; (load-theme 'flatui t)
;; (load-theme 'twilight t)
(if (display-graphic-p)
    (progn
      ;; Use alt as the meta key
      (when (equal system-type 'darwin)
        (setq mac-option-modifier nil)
        (setq mac-command-modifier 'meta)
        (setq mac-pass-command-to-system nil))
      ;; Make solarized do not modify org-mode font size
      ;; (setq solarized-scale-org-headlines nil)
      ;; (setq solarized-use-variable-pitch nil)
      (add-hook 'after-init-hook '(lambda ()
                                    (interactive)
                                    (load-theme 'dracula t)
                                    ))
      (nyan-mode t)
      (setq neo-theme 'icons))
  (setq neo-theme 'arrow)
  (add-hook 'after-init-hook '(lambda ()
                                (interactive)
                                (load-theme 'molokai)))
  ;; (when (equal system-type 'darwin)
  ;;   (setq mac-option-modifier 'meta)
  ;;   (setq mac-command-modifier nil))
  )

;; Ignore case in eshell
(setq eshell-cmpl-ignore-case t)

;; Remap C-g (ESC does the same)
(global-unset-key (kbd "C-g"))
(global-unset-key (kbd "C-h"))

;; Border line between windows
;;(set-face-attribute 'vertical-border nil :foreground "#282a2e")
(set-face-attribute 'vertical-border nil :foreground "black")

;; Enable flyspell for text and string and comments when programming
(require 'flyspell-lazy)
(flyspell-lazy-mode 1)

(add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
(add-hook 'markdown-mode-hook (lambda ()
                                (interactive)
                                (flyspell-mode)
                                (flyspell-buffer)
                                (turn-on-auto-fill)
                                (set-fill-column 80)))
(setq markdown-gfm-use-electric-backquote nil)
(setq auto-mode-alist (cons '("\\.md$" . gfm-mode) auto-mode-alist))

;; Apiary blueprint mode apib-mode
(autoload 'apib-mode "apib-mode"
        "Major mode for editing API Blueprint files" t)
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

;; Don't use escape sequence in visual or magit modes
(setq evil-escape-inhibit-functions '(evil-visual-state-p
                                      zz-magit-status-p
                                      zz-compilation-mode-p
                                      zz-help-mode-p))

;; Reconocer "_" como parte de la palabra
;; (with-eval-after-load 'evil
;;     (defalias #'forward-evil-word #'forward-evil-symbol))

;; ============================================================================

;; Git Gutter
(global-git-gutter-mode +1)
(setq git-gutter:hide-gutter t)

;; Run after magit commit or reverts
(add-hook 'git-gutter:update-hooks 'magit-after-revert-hook)
(add-hook 'git-gutter:update-hooks 'magit-not-reverted-hook)

;; (setq git-gutter:ask-p nil) Don't ask stagin or reverting
(setq git-gutter:modified-sign " ~")
(setq git-gutter:added-sign " +")
(setq git-gutter:deleted-sign " -")
(setq git-gutter:window-width 3)

;; Temporal fix background becomes foreground https://github.com/syohex/emacs-git-gutter/issues/141
;; (set-face-background 'git-gutter:modified "#b58900")
;; (set-face-background 'git-gutter:added "#859900")
;; (set-face-background 'git-gutter:deleted "#dc322f")
;; (set-face-foreground 'git-gutter:modified "none")
;; (set-face-foreground 'git-gutter:added "none")
;; (set-face-foreground 'git-gutter:deleted "none")
(set-face-foreground 'git-gutter:modified "#b58900")
(set-face-foreground 'git-gutter:added "#859900")
(set-face-foreground 'git-gutter:deleted "#dc322f")

(set-face-font 'git-gutter:modified "Menlo")
(set-face-font 'git-gutter:added "Menlo")
(set-face-font 'git-gutter:deleted "Menlo")

;; ============================================================================

(require 'highlight-chars)
;; (set-face-attribute 'hc-tab nil :background nil :foreground "#333333")
(set-face-attribute 'hc-tab nil :background nil :foreground "#586e75")
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)

(require 'whitespace)
(setq whitespace-style '(face trailing tab-mark lines-tail))
;; (setq whitespace-display-mappings '((tab-mark     ?\t    [?\u25b6 ?\t] [?\\ ?\t])
;; ;;                                     ;; this adds eol as in vim needs
;; ;;                                     ;; newline-mark on whitespace-style
;; ;;                                     ;; (newline-mark ?\n    [?\u00ac ?\n])
;; ;;                                     ))

(add-hook 'prog-mode-hook '(lambda ()
                            (interactive)
                            (whitespace-mode 0)
                            (setq whitespace-line-column 80)
                            (whitespace-mode 1)))

(add-hook 'python-mode-hook '(lambda ()
                            (interactive)
                            (whitespace-mode 0)
                            (setq whitespace-line-column 79)
                            (whitespace-mode 1)))

(add-hook 'web-mode-hook '(lambda ()
                           (interactive)
                           (turn-off-auto-fill)
                           (whitespace-mode 0)
                           (setq whitespace-line-column 101)
                           (whitespace-mode 1)))

(add-hook 'csharp-mode-hook '(lambda ()
                              (interactive)
                              (whitespace-mode 0)
                              (setq whitespace-line-column 101)
                              (whitespace-mode 1)))

;; Autowrap lines when reaching the line column
(add-hook 'prog-mode-hook (lambda() (interactive) (turn-on-auto-fill) (set-fill-column 80)))
(add-hook 'python-mode-hook (lambda() (interactive) (turn-on-auto-fill) (set-fill-column 79)))
(add-hook 'csharp-mode-hook (lambda () (interactive) (turn-off-auto-fill) (set-fill-column 100)))

;; ESS mode (R mode)
(add-hook 'ess-mode-hook 'smartparens-mode)

;; Octave mode
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))  ;; asoc .m with octave

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; Elisp mode
(require 'evil-lispy)

(add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
(add-hook 'clojure-mode-hook #'evil-lispy-mode)

;; ============================================================================

;;; Up & Down in Helm
(define-key helm-map (kbd "C-k") 'helm-next-line)
(define-key helm-map (kbd "C-j") 'helm-previous-line)
(define-key helm-map (kbd "TAB") 'helm-next-source)

;;; Messages buffer Mappings
(evil-define-key 'normal messages-buffer-mode-map (kbd "gb") 'evil-buffer)
(evil-define-key 'normal messages-buffer-mode-map (kbd "gt") 'eyebrowse-next-window-config)

;;; Dired Mode Mappings
(evil-define-key 'normal dired-mode-map (kbd "o") 'dired-find-alternate-file)
(evil-define-key 'normal dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(evil-define-key 'normal dired-mode-map (kbd "u") 'zz-dired-up-directory)
(evil-define-key 'normal dired-mode-map (kbd "C-r") 'revert-buffer)
(evil-define-key 'normal dired-mode-map (kbd "r") 'dired-do-redisplay)
(evil-define-key 'normal dired-mode-map (kbd "k") 'dired-next-line)
(evil-define-key 'normal dired-mode-map (kbd "j") 'dired-previous-line)
(evil-define-key 'normal dired-mode-map (kbd "M-{") 'evil-prev-buffer)
(evil-define-key 'normal dired-mode-map (kbd "M-}") 'evil-next-buffer)
(evil-define-key 'normal dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
(evil-define-key 'normal dired-mode-map (kbd "<backtab>") 'dired-subtree-cycle)
(evil-define-key 'normal dired-mode-map (kbd "gb") 'evil-buffer)
(evil-define-key 'normal dired-mode-map (kbd "gt") 'eyebrowse-next-window-config)


(progn
  ;; Define prefix keymap (for custom dired mappings)
  (define-prefix-command 'zz-dired-mode-key-map)
  (define-key zz-dired-mode-key-map (kbd "c") 'dired-do-copy)
  (define-key zz-dired-mode-key-map (kbd "m") 'dired-do-rename)
  (define-key zz-dired-mode-key-map (kbd "a") 'dired-create-directory)
  (define-key zz-dired-mode-key-map (kbd "d") 'dired-do-delete)
  ;;(define-key zz-dired-mode-key-map (kbd "l") 'dired-symlink-this-file)
  ;;(define-key zz-dired-mode-key-map (kbd "p") 'dired-chmod-this-file)
  ;;(define-key zz-dired-mode-key-map (kbd "o") 'dired-chown-this-file)
  ;;(define-key zz-dired-mode-key-map (kbd "g") 'dired-chgrp-this-file)
  ;;(define-key zz-dired-mode-key-map (kbd "t") 'dired-touch-this-file)
  ;;(define-key zz-dired-mode-key-map (kbd "z") 'dired-compress-this-file)
  ;;(define-key zz-dired-mode-key-map (kbd "s") 'dired-sign-this-file)
  ;;(define-key zz-dired-mode-key-map (kbd "v") 'dired-verify-this-file)
  )

(define-key dired-mode-map (kbd "m") zz-dired-mode-key-map)

;;=============================================================================
;; Evil leader
(global-evil-leader-mode)
(setq evil-leader/in-all-states 1)
(evil-leader/set-leader "<SPC>")

;; Evil leader mappings
(evil-leader/set-key "mx" 'helm-M-x)
(evil-leader/set-key "sh" 'zz-eshell-current-dir)
(evil-leader/set-key "zsh" 'zz-zshell-current-dir)
(evil-leader/set-key "tl" 'neotree-toggle)
(evil-leader/set-key "tt" 'zz-toggle-dired)
(evil-leader/set-key "tr" 'zz-toggle-projectile-dired)
(evil-leader/set-key "ls" 'helm-buffers-list)
(evil-leader/set-key "ta" 'align-regexp)
(evil-leader/set-key "ff" 'find-file)

;; Evil leader flyspell mappings
(evil-leader/set-key "ss" (lambda () (interactive) (ispell-change-dictionary "espanol") (flyspell-buffer)))
(evil-leader/set-key "se" (lambda () (interactive) (ispell-change-dictionary "english") (flyspell-buffer)))
(evil-leader/set-key "fl" 'flyspell-auto-correct-previous-word)
(evil-leader/set-key "fs" 'helm-flyspell-correct)

;; Evil leader helper mappings
(evil-leader/set-key "hk" 'describe-key)
(evil-leader/set-key "hm" 'describe-mode)
(evil-leader/set-key "hv" 'describe-variable)
(evil-leader/set-key "hf" 'describe-function)
(evil-leader/set-key "ha" 'apropos-command)
(evil-leader/set-key "hd" 'apropos-documentation)
(evil-leader/set-key "hi" 'info)

;; Evil leader magit mappings
(evil-leader/set-key "gs" 'magit-status)
(evil-leader/set-key "gco" 'zz-magit-branch-and-checkout)
(evil-leader/set-key "go" 'zz-magit-checkout)
(evil-leader/set-key "gl" 'magit-log-current)
(evil-leader/set-key "gb" 'magit-blame)
(evil-leader/set-key "gm" 'magit-merge)
(evil-leader/set-key "gpl" 'magit-pull)
(evil-leader/set-key "gps" 'magit-push)
(evil-leader/set-key "mgr" 'magit-ediff-resolve)

;; Org leader mappings
(evil-leader/set-key "oa" 'org-agenda)
(evil-leader/set-key "ot" 'org-todo)

;; Stages and revert current hunk
(evil-leader/set-key "ga" 'zz-git-gutter-stage-hunk)
(evil-leader/set-key "gr" 'git-gutter:revert-hunk)

;; Center buffer contents
(evil-leader/set-key "cw" 'centered-window-mode)

;; Usar join con "J" en org-mode
(evil-leader/set-key-for-mode 'org-mode "j" 'evil-join)

;; PHPUnit in php-mode
(evil-leader/set-key-for-mode 'php-mode "rr" 'phpunit-current-test)
(evil-leader/set-key-for-mode 'php-mode "rt" 'phpunit-current-class)
(evil-leader/set-key-for-mode 'php-mode "ra" 'phpunit-current-project)

;; Alchemist in elixir-mode
(evil-leader/set-key-for-mode 'elixir-mode "rr" 'alchemist-mix-test-at-point)
(evil-leader/set-key-for-mode 'elixir-mode "rt" 'alchemist-mix-test-file)
(evil-leader/set-key-for-mode 'elixir-mode "ra" 'alchemist-mix-test)

;; Open file in web-mode
(evil-leader/set-key-for-mode 'web-mode "rr" 'browse-url-of-file)

;;=============================================================================
;; Evil (always at end of file)
(require 'evil)
(evil-mode 1)

;; Evil options
(fset 'evil-visual-update-x-selection 'ignore)

;; Evil plugins
(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; Expand visual selection
(define-key evil-visual-state-map (kbd "v") 'er/expand-region)
(define-key evil-visual-state-map (kbd "V") 'er/contract-region)

;; Eshell custom insert binding
(add-hook 'eshell-mode-hook
      '(lambda ()
         (local-set-key (kbd "C-l") 'zz-eshell-clear-buffer)
         (define-key evil-normal-state-local-map (kbd "i") 'zz-eshell-insert)
         (define-key evil-insert-state-local-map (kbd "C-n") 'eshell-next-input)
         (define-key evil-insert-state-local-map (kbd "C-p") 'eshell-previous-input)
         (define-key evil-insert-state-local-map (kbd "C-c C-d") 'zz-eshell-exit)))

(add-hook 'term-mode-hook
          '(lambda ()
             (define-key evil-normal-state-local-map (kbd "i") 'zz-zshell-insert)))

;; Custom insert definition
(define-key evil-normal-state-map (kbd "i") 'zz-evil-insert-state)
(define-key evil-normal-state-map (kbd "I") '(lambda ()
                                               (interactive)
                                               (zz-evil-insert-state "I")))
(define-key evil-normal-state-map (kbd "a") '(lambda ()
                                               (interactive)
                                               (zz-evil-insert-state "a")))
(define-key evil-normal-state-map (kbd "A") '(lambda ()
                                               (interactive)
                                               (zz-evil-insert-state "A")))

;; Exchange buffers
;; (require 'buffer-move)
(setq buffer-move-stay-after-swap t)
(define-key evil-normal-state-map (kbd "C-w K") 'buf-move-down)
(define-key evil-normal-state-map (kbd "C-w H") 'buf-move-up)
(define-key evil-normal-state-map (kbd "C-w J") 'buf-move-left)
(define-key evil-normal-state-map (kbd "C-w L") 'buf-move-right)

;; Escape to emacs mode
;; (define-key evil-motion-state-map (kbd "M-\\") 'evil-execute-in-emacs-state)

;;; Use visual lines and movements keys for colemak
(define-key evil-normal-state-map (kbd "k") 'evil-next-line)
(define-key evil-normal-state-map (kbd "j") 'evil-previous-line)
(define-key evil-visual-state-map (kbd "k") 'evil-next-line)
(define-key evil-visual-state-map (kbd "j") 'evil-previous-line)

;;; Save with C-s (linux style) and M-s (mac style)
(define-key evil-insert-state-map (kbd "C-s") 'zz-save-to-escape)
(define-key evil-insert-state-map (kbd "M-s") 'zz-save-to-escape)
(define-key evil-normal-state-map (kbd "C-s") 'evil-write)
(define-key evil-normal-state-map (kbd "M-s") 'evil-write)

;; Neotree as an IDE
(define-key evil-normal-state-map (kbd "M-1") 'neotree-toggle)
(define-key evil-insert-state-map (kbd "M-1") 'neotree-toggle)

;;; Insert new line at RET
(define-key evil-normal-state-map (kbd "RET") 'newline)

;;; Flycheck nicer mappings
(define-key evil-normal-state-map (kbd "]e") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "[e") 'flycheck-previous-error)

;;; Flyspell nicer mappings
(define-key evil-normal-state-map (kbd "]s") 'flyspell-goto-next-error)
(define-key evil-normal-state-map (kbd "[s") 'flyspell-goto-previous-error)

;; Git Gutter Jump to next/previous hunk
(define-key evil-normal-state-map (kbd "]h") 'git-gutter:next-hunk)
(define-key evil-normal-state-map (kbd "[h") 'git-gutter:previous-hunk)

;; Smerge mode jump to next/previous conflict
; (define-key smerge-mode-map (kbd "]c") 'smerge-next)
; (define-key smerge-mode-map (kbd "[c") 'smerge-prev)

;;; Open init.el as preferences
(define-key evil-normal-state-map (kbd "M-,") (lambda () (interactive) (find-file user-init-file)))

;;; Revert buffer no confirmation
(evil-define-key 'normal prog-mode-map (kbd "gr") (lambda () (interactive) (revert-buffer t t)))
(define-key evil-normal-state-map (kbd "gV") 'zz-evil-select-pasted)

;; Bubble lines
(define-key evil-normal-state-map (kbd "M-h") #'drag-stuff-up)
(define-key evil-normal-state-map (kbd "M-k") #'drag-stuff-down)

;; Copy and paste be natural
(define-key evil-visual-state-map (kbd "M-c") 'evil-yank)
(define-key evil-visual-state-map (kbd "M-v") 'evil-visual-paste)
(define-key evil-insert-state-map (kbd "M-v") 'evil-paste-before)
(define-key evil-normal-state-map (kbd "M-v") 'evil-paste-after)

;;; Org mappings
(add-hook 'org-mode-hook
      '(lambda ()
         (define-key evil-normal-state-local-map (kbd "M-h") 'org-metaup)
         (define-key evil-normal-state-local-map (kbd "M-k") 'org-metadown)
         (define-key evil-normal-state-local-map (kbd "M-j") 'org-metaleft)
         (define-key evil-normal-state-local-map (kbd "M-l") 'org-metaright)
         (define-key evil-insert-state-local-map (kbd "M-h") 'org-metaup)
         (define-key evil-insert-state-local-map (kbd "M-k") 'org-metadown)
         (define-key evil-insert-state-local-map (kbd "M-j") 'org-metaleft)
         (define-key evil-insert-state-local-map (kbd "M-l") 'org-metaright)
         (define-key evil-normal-state-local-map (kbd "H") 'org-shiftup)
         (define-key evil-normal-state-local-map (kbd "K") 'org-shiftdown)
         (define-key evil-normal-state-local-map (kbd "L") 'org-shiftright)
         (define-key evil-normal-state-local-map (kbd "J") 'org-shiftleft)))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-k") 'org-agenda-next-line)
             (local-set-key (kbd "C-h") 'org-agenda-previous-line)))

;;; Compile mode mappings
(add-hook 'compilation-mode-hook
          '(lambda ()
             (local-unset-key (kbd "g"))
             (local-set-key (kbd "j") 'evil-previous-visual-line)
             (local-set-key (kbd "k") 'evil-next-visual-line)
             (local-set-key (kbd "h") 'evil-backward-char)
             (local-set-key (kbd "l") 'evil-forward-char)
             (local-set-key (kbd "gr") 'recompile)
             (local-set-key (kbd "gb") 'evil-buffer)
             (local-set-key (kbd "C-u") 'zz-scroll-half-page-up)
             (local-set-key (kbd "C-d") 'zz-scroll-half-page-down)))

;;; Ctrl+P
(define-key evil-normal-state-map (kbd "M-p") 'zz-find-file)
(define-key evil-normal-state-map (kbd "C-p") 'zz-find-file) ;; Alias
(define-key evil-normal-state-map (kbd "M-P") 'helm-projectile-switch-project)

;; Show buffer file name
(define-key evil-normal-state-map (kbd "ga") 'zz-file-stats)
(define-key evil-normal-state-map (kbd "zv") 'zz-scroll-line-to-quarter)

;; Alias for jump between windows
(define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-w C-j") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-w C-k") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-w h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-w j") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-w k") 'evil-window-down)

;; Frames like tabs?
(define-key evil-normal-state-map (kbd "C-w x") 'other-frame)
(define-key evil-normal-state-map (kbd "C-w f") 'make-frame-command)
(define-key evil-normal-state-map (kbd "C-w O") 'delete-other-frames)

;; Buffer navigation
(define-key evil-normal-state-map (kbd "gb") 'evil-buffer)
(define-key evil-normal-state-map (kbd "M-{") 'evil-prev-buffer)
(define-key evil-normal-state-map (kbd "M-}") 'evil-next-buffer)
(define-key evil-normal-state-map (kbd "C-a k") 'zz-kill-buffer)
(define-key evil-normal-state-map (kbd "C-a c") 'zz-kill-other-buffers)
(define-key evil-normal-state-map (kbd "C-a d") 'zz-evil-window-delete-or-die)

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

;; Fix mappings (they're not by default)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-down)

;; Help Mode Mappings
(define-key help-mode-map (kbd "C-u") 'zz-scroll-half-page-up)

;; Package Mode Mappings
;;; Up & Down
(define-key package-menu-mode-map (kbd "k") 'evil-next-line)
(define-key package-menu-mode-map (kbd "h") 'evil-previous-line)

;;; Scroll Package List
(define-key package-menu-mode-map (kbd "C-u") 'zz-scroll-half-page-up)
(define-key package-menu-mode-map (kbd "C-d") 'zz-scroll-half-page-down)
(define-key package-menu-mode-map (kbd "C-f") 'evil-scroll-page-down)
(define-key package-menu-mode-map (kbd "C-b") 'evil-scroll-page-up)

;;; Search Package List
(define-key package-menu-mode-map (kbd "/") 'evil-search-forward)
(define-key package-menu-mode-map (kbd "?") 'evil-search-backward)
(define-key package-menu-mode-map (kbd "n") 'evil-search-next)
(define-key package-menu-mode-map (kbd "N") 'evil-search-previous)

;;; Emmet mappings
(evil-define-key 'insert html-mode-map (kbd "C-e ,") 'emmet-expand-line)
(evil-define-key 'insert web-mode-map (kbd "C-e ,") 'emmet-expand-line)
(evil-define-key 'insert css-mode-map (kbd "C-e ,") 'emmet-expand-line)
(evil-define-key 'insert nxml-mode-map (kbd "C-e ,") 'emmet-expand-line)

;;; Neotree mappings
;; NerdTree like (move to package later)
(evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-enter)

;; Sane defaults
(evil-define-key 'normal neotree-mode-map (kbd "md") 'neotree-delete-node)
(evil-define-key 'normal neotree-mode-map (kbd "ma") 'neotree-create-node)
(evil-define-key 'normal neotree-mode-map (kbd "mm") 'neotree-rename-node)
(evil-define-key 'normal neotree-mode-map (kbd "C") 'neotree-change-root)
(evil-define-key 'normal neotree-mode-map (kbd "R") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "?") 'describe-mode)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "h") 'describe-mode)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)

; extras
(evil-define-key 'normal neotree-mode-map (kbd "u") 'neotree-select-up-node)

;;; Cider mappings
(evil-define-key 'normal cider-repl-mode-map (kbd "C-a d") 'cider-quit)
(evil-define-key 'insert cider-repl-mode-map (kbd "C-c C-d") 'cider-quit)
(evil-define-key 'insert cider-repl-mode-map (kbd "C-c C-c") 'cider-repl-kill-input)
(evil-define-key 'insert cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
(evil-define-key 'insert cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)
(evil-define-key 'insert cider-repl-mode-map (kbd "C-p") 'cider-repl-previous-input)
(evil-define-key 'insert cider-repl-mode-map (kbd "C-n") 'cider-repl-next-input)
(evil-define-key 'insert cider-repl-mode-map (kbd "C-j") 'cider-repl-previous-input)
(evil-define-key 'insert cider-repl-mode-map (kbd "C-k") 'cider-repl-next-input)
(evil-define-key 'insert cider-repl-mode-map (kbd "C-l") 'cider-repl-clear-buffer)
(evil-define-key 'normal cider-docview-mode-map (kbd "q") 'zz-kill-buffer)
(evil-define-key 'normal cider-stacktrace-mode-map (kbd "q") 'zz-kill-buffer)
(add-hook 'cider-repl-mode-hook
          '(lambda ()
             (evil-leader/set-key "hd" 'cider-doc)))

;;; Marks Package List
(evil-define-key 'normal package-menu-mode-map (kbd "o") 'package-menu-describe-package
                                               (kbd "i") 'package-menu-mark-install
                                               (kbd "d") 'package-menu-mark-delete
                                               (kbd "u") 'package-menu-mark-unmark
                                               (kbd "U") 'package-menu-mark-upgrades
                                               (kbd "r") 'package-menu-refresh
                                               (kbd "x") 'package-menu-execute
                                               (kbd "q") 'quit-window)
;; Evil ex commands
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "ls" 'helm-buffers-list))
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "fly[check]" 'flycheck-list-errors))
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "full[screen]" 'toggle-frame-fullscreen))
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "totab[s]" 'tabify))
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "tospace[s]" 'untabify))
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "tab[s]" '(lambda ()
                                   (interactive)
                                   (setq indent-tabs-mode t))))
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "space[s]" '(lambda ()
                                     (interactive)
                                     (setq indent-tabs-mode nil))))
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "proj[ectile]" 'helm-projectile-switch-project))
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "cider[-repl]" 'cider-jack-in))
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "Git cl[one]" 'magit-clone))

;; Exit insert mode by pressing j and then j quickly
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence ";q")
(setq-default evil-escape-delay 0.3)
(global-set-key (kbd "<escape>") 'evil-escape)

;; Use emacs mode in these modes
(evil-set-initial-state 'org-agenda-mode 'emacs)
(evil-set-initial-state 'package-menu-mode 'emacs)
(add-hook 'git-commit-mode-hook 'evil-insert-state)

;; Overrides Evil
;; =============================================================================

;; C-w deletes word in minibuffer
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "M-v") 'clipboard-yank)

;; Evil Magit
(require 'evil-magit)
(evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward)
(evil-define-key evil-magit-state magit-mode-map "gr" 'magit-refresh)

;; Evil Snipe
(require 'evil-snipe)
(evil-snipe-mode 1)
(evil-snipe-override-mode 1)
(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
(setq evil-snipe-spillover-scope 'whole-visible)
(setq evil-snipe-show-prompt nil)

;; Eyebrowse
(eyebrowse-mode t)
(eyebrowse-setup-opinionated-keys)
(setq eyebrowse-wrap-around t)
(setq eyebrowse-new-workspace t)
(setq spaceline-workspace-numbers-unicode t)

;; Close workspace with gc

(evil-leader/set-key "t0" 'eyebrowse-switch-to-window-config-0)
(evil-leader/set-key "t1" 'eyebrowse-switch-to-window-config-1)
(evil-leader/set-key "t2" 'eyebrowse-switch-to-window-config-2)
(evil-leader/set-key "t3" 'eyebrowse-switch-to-window-config-3)
(evil-leader/set-key "t4" 'eyebrowse-switch-to-window-config-4)
(evil-leader/set-key "t5" 'eyebrowse-switch-to-window-config-5)
(evil-leader/set-key "t6" 'eyebrowse-switch-to-window-config-6)
(evil-leader/set-key "t7" 'eyebrowse-switch-to-window-config-7)
(evil-leader/set-key "t8" 'eyebrowse-switch-to-window-config-8)
(evil-leader/set-key "t9" 'eyebrowse-switch-to-window-config-9)

;; Temporales
;; =============================================================================
;; fullscreen, maximized, fullboth
(if (display-graphic-p)
    (progn
      (when (zz-on-asus)
        ;; Obtener medidas con (frame-width) (frame-height)
        (setq cwm-centered-window-width 100)
        (add-to-list 'default-frame-alist '(left . 0))
        (add-to-list 'default-frame-alist '(top . 0))
        (add-to-list 'default-frame-alist '(width . 225))
        (add-to-list 'default-frame-alist '(height . 55)))
      (when (zz-on-pro)
        (setq cwm-centered-window-width 90)
        (add-to-list 'default-frame-alist '(left . 0))
        (add-to-list 'default-frame-alist '(top . 0))
        (add-to-list 'default-frame-alist '(width . 139))
        (add-to-list 'default-frame-alist '(height . 37)))))

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . fullscreen))

;; Dont see these minor modes in the mode line
(require 'diminish)
(diminish 'helm-mode)
(diminish 'evil-escape-mode)
(diminish 'undo-tree-mode)
(diminish 'projectile-mode)
(diminish 'auto-fill-function)
(diminish 'whitespace-mode)
(diminish 'git-gutter-mode)
(diminish 'flycheck-mode)
(diminish 'snipe)

;; ========= Temproales
;; =============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centered-window-mode nil)
 '(custom-safe-themes
   (quote
    ("417a047001847a55f9e0d9692f2bde644a325ab8a1ef18b22baea8309d9164cb" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "12670281275ea7c1b42d0a548a584e23b9c4e1d2dabb747fd5e2d692bcd0d39b" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "24685b60b28b071596be6ba715f92ed5e51856fb87114cbdd67775301acf090d" "f23a961abba42fc5d75bf94c46b5688c52683c02b3a81313dd0738b4d48afd1d" "5673c365c8679addfb44f3d91d6b880c3266766b605c99f2d9b00745202e75f6" "8d3c5e9ba9dcd05020ccebb3cc615e40e7623b267b69314bdb70fe473dd9c7a8" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "4605ce6e798971d215b01844ea39e993d683aa2fa118e02e263539298f9f3921" "92192ea8f0bf04421f5b245d906701abaa7bb3b0d2b3b14fca2ee5ebb1da38d8" "f984e2f9765a69f7394527b44eaa28052ff3664a505f9ec9c60c088ca4e9fc0b" "d83e34e28680f2ed99fe50fea79f441ca3fddd90167a72b796455e791c90dc49" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "f869a5d068a371532c82027cdf1feefdc5768757c78c48a7e0177e90651503ad" "d29231b2550e0d30b7d0d7fc54a7fb2aa7f47d1b110ee625c1a56b30fea3be0f" "a632c5ce9bd5bcdbb7e22bf278d802711074413fd5f681f39f21d340064ff292" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "b25040da50ef56b81165676fdf1aecab6eb2c928fac8a1861c5e7295d2a8d4dd" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "357d5abe6f693f2875bb3113f5c031b7031f21717e8078f90d9d9bc3a14bcbd8" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "d9850d120be9d94dd7ae69053630e89af8767c36b131a3aa7b06f14007a24656" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4" "076a94693c0f6fa99612121c18ccb48bfbd842c05b6b9ed04b6e7e0a0f95a53e" "0a4d0f951ce441b593a8ebeb63b2f36c93db6051a993a9b8f4774feacb620b2e" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "85d609b07346d3220e7da1e0b87f66d11b2eeddad945cac775e80d2c1adb0066" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "03e3e79fb2b344e41a7df897818b7969ca51a15a67dc0c30ebbdeb9ea2cd4492" "101a10b15bbbd0d5a0e56e4773e614962197886780afb2d62523a63a144ad96c" "82cbb553a225b75ee49901fa06562941fbfe5e6fed24cda985e7ea59af7ddc80" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "3fb38c0c32f0b8ea93170be4d33631c607c60c709a546cb6199659e6308aedf7" "0b6645497e51d80eda1d337d6cabe31814d6c381e69491931a688836c16137ed" "aed73c6d0afcf2232bb25ed2d872c7a1c4f1bda6759f84afc24de6a1aec93da8" "232f715279fc131ed4facf6a517b84d23dca145fcc0e09c5e0f90eb534e1680f" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "8ffaf449297bd9a08517f4b03a4df9dbf3e347652746cefceb3ee57c8e584b9f" "01c5ebefcabc983c907ee30e429225337d0b4556cc1d21df0330d337275facbb" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "2f4afdef79a7f8a6b54f7e70959e059d7e09cf234d412662e0897cacd46f04b4" "294834baa9ca874795a3181cce7aaf228b1e3fb3899587ffd3ae7546de328c90" "3a3917dbcc6571ef3942c2bf4c4240f70b5c4bc0b28192be6d3f9acd83607a24" "1a2b131a7844bad234832963d565097efc88111b196fb75757885c159c5f8137" "cdfb22711f64d0e665f40b2607879fcf2607764b2b70d672ddaa26d2da13049f" "135bbd2e531f067ed6a25287a47e490ea5ae40b7008211c70385022dbab3ab2a" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "50e7f9d112e821e42bd2b8410d50de966c35c7434dec12ddea99cb05dd368dd8" default)))
 '(inhibit-startup-screen t)
 '(org-export-backends (quote (ascii html icalendar latex md)))
 '(package-selected-packages
   (quote
    (sbt-mode scala-mode go-eldoc company-go dockerfile-mode elm-mode disable-mouse gruvbox-theme seti-theme dracula-theme ox-reveal org highlight-chars go-mode apib-mode feature-mode twilight-anti-bright-theme twilight-bright-theme twilight-theme flatui-theme centered-window-mode evil-lispy ess cask-mode evil-embrace expand-region smartparens exec-path-from-shell oceanic-theme apropospriate-theme keyfreq company-tern rbenv yahoo-weather all-the-icons all-the-icons-dired apache-mode yaml-mode coffee-mode alchemist neotree csharp-mode yasnippet window-numbering web-mode theme-changer spaceline solarized-theme request-deferred js2-mode evil-anzu company-web json-mode python-environment phpunit php-mode ox-gfm org-bullets nyan-mode molokai-theme material-theme markdown-mode helm-projectile helm-flyspell git-gutter flyspell-lazy flycheck flx fancy-battery eyebrowse evil-visualstar evil-vimish-fold evil-surround evil-snipe evil-matchit evil-magit evil-leader evil-escape epc emmet-mode drag-stuff dired-subtree diminish company clojure-mode-extra-font-locking cider buffer-move base16-theme)))
 '(safe-local-variable-values
   (quote
    ((eval flycheck-set-checker-executable
           (quote python-flake8)
           (concat
            (projectile-project-root)
            ".venv/bin/flake8"))
     (eval flycheck-set-checker-executable
           (concat
            (projectile-project-root)
            ".venv/bin/flake8"))
     (eval progn
           (pythonic-activate
            (concat
             (projectile-project-root)
             ".venv")))
     (flycheck-phpcs-standard concat
                              (projectile-project-root)
                              "phpcs.xml")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#282828"))))
 '(mode-line ((t (:background "#000000" :foreground "#F8F8F2" :box (:line-width 1 :color "#000000" :style released-button) :overline "#073642" :underline "#284b54" :height 1))))
 '(mode-line-inactive ((t (:background "#000000" :foreground "#BCBCBC" :box (:line-width 1 :color "#002b36" :style unspecified) :overline "#073642" :underline "#284b54" :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Hack")))))
(put 'dired-find-alternate-file 'disabled nil)
;;; init.el ends here
